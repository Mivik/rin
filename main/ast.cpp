
#include <set>

#include "ast.h"
#include "parser.h"
#include "ref.h"
#include "tfunc.h"

namespace rin {

using K = TokenKind;

[[noreturn]] void not_const_evaluated(Codegen &g, const ASTNode *node) {
	// TODO wtf is this
	g.error(
		"Part of the code ({} ~ {}) is not const evaluated",
		node->get_source_range().begin, node->get_source_range().end
	);
}

Value ArrayTypeNode::codegen(Codegen &g) const {
	auto length_value = length_node->codegen(g);
	auto constant = llvm::dyn_cast<llvm::ConstantInt>(length_value.get_llvm_value());
	return Value(g.get_context().get_array_type(
		sub_type_node->codegen(g).get_type_value(),
		static_cast<uint32_t>(constant->getZExtValue())
	));
}

BlockNode::BlockNode(const SourceRange &range, std::vector<Ptr<ASTNode>> stmts):
	ASTNode(range),
	stmt_nodes(std::move(stmts)) {
	auto iter = this->stmt_nodes.begin();
	while (iter != this->stmt_nodes.end() && !dynamic_cast<ReturnNode *>(iter->get()))
		++iter;
	// TODO better handling here?
	if (iter != this->stmt_nodes.end())
		this->stmt_nodes.erase(++iter, this->stmt_nodes.end());
	has_return_flag = !this->stmt_nodes.empty() && this->stmt_nodes.back()->has_return();
}

VarDeclNode::VarDeclNode(
	const SourceRange &range,
	std::string name,
	Ptr<ASTNode> type_node,
	Ptr<ASTNode> value_node,
	bool is_mutable,
	bool is_inline
):
	ASTNode(range), name(std::move(name)),
	type_node(std::move(type_node)),
	value_node(std::move(value_node)),
	mutable_flag(is_mutable),
	inline_flag(is_inline) {
	if (!(this->type_node || this->value_node))
		throw ParseException("A variable should have either a default value or a type annotation");
	if (!is_mutable && !this->value_node)
		throw ParseException("The default value of const variable should be given");
}

Value ConstantNode::codegen(Codegen &g) const {
	auto &ctx = g.get_context();
	if (content == "true" || content == "false") {
		auto type = ctx.get_boolean_type();
		return {
			type,
			direct_cast<llvm::Value>(llvm::ConstantInt::get(type->get_llvm(), content == "true"))
		};
	}
	if (isdigit(content[0])) {
		auto tmp = content;
		assert(!tmp.empty());
		auto type = ctx.get_i32_type();
		if (tolower(tmp.back()) == 'l') {
			tmp.pop_back();
			if (!tmp.empty() && tolower(tmp.back()) == 'l')
				tmp.pop_back();
			assert(!tmp.empty());
			if (tolower(tmp.back()) == 'u') {
				tmp.pop_back();
				type = ctx.get_u64_type();
			} else type = ctx.get_i64_type();
		} else if (tolower(tmp.back()) == 'u') {
			tmp.pop_back();
			assert(!tmp.empty());
			type = ctx.get_u32_type();
		}
		return { type, direct_cast<llvm::Value>(
			llvm::ConstantInt::get(
				direct_cast<llvm::IntegerType>(type->get_llvm()),
				tmp, 10
			)
		) };
	}
	g.error("Unknown constant: {}", content);
}

Value ValueNode::codegen(Codegen &g) const {
	auto opt = g.lookup_value(name);
	if (!opt.has_value()) g.error("Use of undeclared value: {}", name);
	if (g.is_const_eval() && !opt->is_constant()) not_const_evaluated(g, this);
	return preserve_ref? *opt: opt->deref(g);
}

Value UnaryOpNode::codegen(Codegen &g) const {
	auto unary_op_fail = [&g](const Value &value, K kind) {
		g.error(
			"Illegal unary operation on {}: {}",
			value.get_type()->to_string(),
			token_kind::name(kind)
		);
	};

	auto &builder = *g.get_builder();
	value_node->preserve_reference();
	auto value = value_node->codegen(g);
	// TODO remove this
	assert(!g.is_const_eval() || value.is_constant());
	// TODO const pointer/reference
	// TODO array type
	switch (op) {
		case K::Pointer:
		case K::PointerMut:
			if (value.deref(g).is_type_value())
				return Value(g.get_context().get_pointer_type(
					value.get_type_value(),
					op == K::Pointer
				));
			else {
				auto v = value.deref(g);
				if (auto ptr_type = dynamic_cast<Type::Pointer *>(v.get_type())) {
					if (op == K::PointerMut && !ptr_type->is_mutable())
						g.error(
							"Attempt to get variable reference to const pointer: {}",
							ptr_type->to_string()
						);
					return g.create_ref_value(
						g.get_context().get_ref_type(ptr_type->get_sub_type(), op == K::Pointer),
						v.get_llvm_value()
					);
				}
				else unary_op_fail(value, op);
			}
			break;
		case K::Ref:
		case K::RefMut:
			if (value.deref(g).is_type_value())
				return Value(g.get_context().get_ref_type(
					value.get_type_value(),
					op == K::Ref
				));
			else if (value.is_ref_value()) {
				// TODO get address
				/*if (auto ref = dynamic_cast<Ref::Address *>(value.get_ref_value())) {
					auto ref_type = ref->get_type();
					return {
						g.get_context().get_pointer_type(ref_type->get_sub_type(), ref_type->is_const()),
						ref->get_address()
					};
				} else
					g.error(
						"Cannot get the address of an abstract reference: {}",
						value.get_type()->to_string()
					);*/
				if (op == K::RefMut) unary_op_fail(value, op);
				return value;
			} else unary_op_fail(value, op);
			break;
		default:
			break;
	}
	value = value.deref(g);
	auto type = value.get_type();
	if (dynamic_cast<Type::Real *>(type)) {
		switch (op) {
			case K::UAdd:
				return value;
			case K::USub:
				return { type, builder.CreateFNeg(value.get_llvm_value()) };
			default:
				unary_op_fail(value, op);
		}
	} else if (dynamic_cast<Type::Int *>(value.get_type())) {
		switch (op) {
			case K::UAdd:
				return value;
			case K::USub:
				return { type, builder.CreateNeg(value.get_llvm_value()) };
			case K::Not:
				return { type, builder.CreateNot(value.get_llvm_value()) };
			default:
				unary_op_fail(value, op);
		}
	} else if (value.get_type() == g.get_context().get_boolean_type()) {
		if (op == K::LNot) return { type, builder.CreateNot(value.get_llvm_value()) };
		else unary_op_fail(value, op);
	} else unary_op_fail(value, op);
	RIN_UNREACHABLE();
}

[[noreturn]] inline void bin_op_fail(Codegen &g, const Value &lhs, const Value &rhs, TokenKind kind) {
	g.error(
		"Illegal binary operation on {} and {}: {}",
		lhs.get_type()->to_string(),
		rhs.get_type()->to_string(),
		token_kind::name(kind)
	);
}

inline Value bin_op_arithmetic_codegen(Codegen &g, Value lhs, Value rhs, TokenKind op) {
	lhs = lhs.deref(g);
	rhs = rhs.deref(g);

	auto type = lhs.get_type();
	if (rhs.get_type() != type) bin_op_fail(g, lhs, rhs, op);

	using bin_op = llvm::Instruction::BinaryOps;
	using cmp_op = llvm::CmpInst::Predicate;

	const bool accept_real = op == K::Add || op == K::Sub || op == K::Mul || op == K::Div;
	const bool result_is_real =
		(dynamic_cast<Type::Real *>(lhs.get_type())
		 || dynamic_cast<Type::Real *>(rhs.get_type()));
	if (!accept_real && result_is_real) bin_op_fail(g, lhs, rhs, op);

	auto real_result_type = dynamic_cast<Type::Real *>(type);
	auto int_result_type = dynamic_cast<Type::Int *>(type);
	const auto bool_type = g.get_context().get_boolean_type();
	auto &builder = *g.get_builder();

	if (!real_result_type && !int_result_type && type != bool_type)
		bin_op_fail(g, lhs, rhs, op);

	cmp_op cmp = cmp_op::FCMP_FALSE;
	auto branch = [&](cmp_op for_signed, cmp_op for_unsigned, cmp_op for_real) {
		if (type == bool_type) cmp = for_unsigned;
		else cmp = real_result_type? for_real: (int_result_type->is_signed()? for_signed: for_unsigned);
	};
	// TODO simplify this shit
	switch (op) {
		case K::Lt:
			branch(cmp_op::ICMP_SLT, cmp_op::ICMP_ULT, cmp_op::FCMP_OLT);
			break;
		case K::Le:
			branch(cmp_op::ICMP_SLE, cmp_op::ICMP_ULE, cmp_op::FCMP_OLE);
			break;
		case K::Gt:
			branch(cmp_op::ICMP_SGT, cmp_op::ICMP_UGT, cmp_op::FCMP_OGT);
			break;
		case K::Ge:
			branch(cmp_op::ICMP_SGE, cmp_op::ICMP_UGE, cmp_op::FCMP_OGE);
			break;
		case K::Eq:
			branch(cmp_op::ICMP_EQ, cmp_op::ICMP_EQ, cmp_op::FCMP_OEQ);
			break;
		case K::Neq:
			branch(cmp_op::ICMP_NE, cmp_op::ICMP_NE, cmp_op::FCMP_ONE);
			break;
		default:
			break;
	}
	if (cmp != cmp_op::FCMP_FALSE) {
		if (real_result_type)
			return { bool_type, builder.CreateFCmp(cmp, lhs.get_llvm_value(), rhs.get_llvm_value()) };
		else
			return { bool_type, builder.CreateICmp(cmp, lhs.get_llvm_value(), rhs.get_llvm_value()) };
	}

	bin_op llvm_op;
	if (real_result_type) {
		switch (op) {
			case K::Add:
				llvm_op = bin_op::FAdd;
				break;
			case K::Sub:
				llvm_op = bin_op::FSub;
				break;
			case K::Mul:
				llvm_op = bin_op::FMul;
				break;
			case K::Div:
				llvm_op = bin_op::FDiv;
				break;
			default:
				g.error("Illegal binary operations on real types: {}", token_kind::name(op));
		}
	} else if (int_result_type && int_result_type->get_bit_width() != 1) {
		switch (op) {
			case K::Add:
				llvm_op = bin_op::Add;
				break;
			case K::Sub:
				llvm_op = bin_op::Sub;
				break;
			case K::Mul:
				llvm_op = bin_op::Mul;
				break;
			case K::Xor:
				llvm_op = bin_op::Xor;
				break;
			case K::Or:
				llvm_op = bin_op::Or;
				break;
			case K::And:
				llvm_op = bin_op::And;
				break;
			case K::Shl:
				llvm_op = bin_op::Shl;
				break;
			case K::Shr:
				llvm_op = int_result_type->is_signed()? bin_op::AShr: bin_op::LShr;
				break;
			case K::Div:
				llvm_op = int_result_type->is_signed()? bin_op::SDiv: bin_op::UDiv;
				break;
			case K::Mod:
				llvm_op = int_result_type->is_signed()? bin_op::SRem: bin_op::URem;
				break;
			default:
				bin_op_fail(g, lhs, rhs, op);
		}
	} else { // logic operators
		// TODO maybe more cases here
		assert(int_result_type);
		switch (op) {
			case K::Or:
				llvm_op = bin_op::Or;
				break;
			case K::And:
				llvm_op = bin_op::And;
				break;
			default:
				bin_op_fail(g, lhs, rhs, op);
		}
	}
	return
		{
			type,
			builder.CreateBinOp(llvm_op, lhs.get_llvm_value(), rhs.get_llvm_value())
		};
}

inline Value assignment_codegen(Codegen &g, Value lhs, Value rhs, TokenKind op) {
	if (!lhs.is_ref_value())
		g.error(
			"The left side of assignment statement must be a reference, got {}",
			lhs.get_type()->to_string()
		);
	auto ref = lhs.get_ref_value();
	auto ref_type = ref->get_type();
	if (!ref_type->is_mutable())
		g.error("Attempt to assign to a const variable");
	TokenKind bop = K::Assign;
	switch (op) {
#define H(s) case K::s##A: bop = K::s; break;
		H(Add)
		H(Sub)
		H(Mul)
		H(Div)
		H(Mod)
		H(Shl)
		H(Shr)
		H(Or)
		H(And)
		H(Xor)
#undef H
		case K::Assign:
			break;
		default:
			RIN_UNREACHABLE();
	}
	rhs = rhs.deref(g);
	if (ref_type->get_sub_type() != rhs.get_type())
		g.error(
			"Attempt to assign a {} to a variable of type {}",
			rhs.get_type()->to_string(),
			ref_type->get_sub_type()->to_string()
		);
	auto value =
		bop == K::Assign
		? rhs
		: bin_op_arithmetic_codegen(g, ref->load(g), rhs, bop);
	assert(value.get_type() == ref_type->get_sub_type());
	ref->store(g, value);
	return lhs;
}

Value BinOpNode::codegen(Codegen &g) const {
	if (op == K::Period) {
		if (auto call_node = dynamic_cast<CallNode *>(rhs_node.get())) {
			assert(!call_node->receiver_node);
			call_node->receiver_node = std::make_unique<PhonyASTNode>(lhs_node->codegen(g));
			return call_node->codegen(g);
		}
		auto value_node = dynamic_cast<ValueNode *>(rhs_node.get());
		if (!value_node) g.error("Accessing invalid member of struct"); // TODO error message
		auto name = value_node->get_name();
		value_node->preserve_reference();
		auto lhs = lhs_node->codegen(g);
		Type::Struct *struct_type = nullptr;
		auto ref_type = dynamic_cast<Type::Ref *>(lhs.get_type());
		if (ref_type)
			struct_type = dynamic_cast<Type::Struct *>(ref_type->get_sub_type());
		if (!struct_type)
			g.error("Left operand of accessing operator is not a reference to struct");
		size_t index;
		if (auto opt = struct_type->find_index_by_name(name)) index = *opt;
		else g.error("Unknown member of struct: {}", name);
		return Value(lhs.get_ref_value()->get_element(g, 0, index));
	}
	lhs_node->preserve_reference();
	auto lhs = lhs_node->codegen(g);
	if (op == K::LBracket)
		if (auto ref_type = dynamic_cast<Type::Ref *>(lhs.get_type()))
			if (dynamic_cast<Type::Tuple *>(ref_type->get_sub_type())) {
				g.push_const_eval();
				auto rhs = rhs_node->codegen(g);
				g.pop_const_eval();
				auto constant = llvm::dyn_cast<llvm::ConstantInt>(rhs.get_llvm_value());
				auto index = constant->getZExtValue();
				return Value(lhs.get_ref_value()->get_element(g, 0, index));
			}
	auto rhs = rhs_node->codegen(g);
	// TODO remove this
	assert(!g.is_const_eval() || (lhs.is_constant() && rhs.is_constant()));
	switch (op) {
		case K::Assign:
		case K::AddA:
		case K::SubA:
		case K::MulA:
		case K::DivA:
		case K::ModA:
		case K::ShlA:
		case K::ShrA:
		case K::OrA:
		case K::AndA:
		case K::XorA:
			return assignment_codegen(g, lhs, rhs, op);
		case K::LBracket: { // pointer subscript
			// TODO we assume that reference is always not const, but is it?
			// TODO safe mode (check in-bound)
			if (lhs.is_ref_value())
				return lhs.get_ref_value()->get_element(g, rhs.deref(g))->load(g);
			if (auto ref_type = dynamic_cast<Type::Ref *>(lhs.get_type()))
				if (dynamic_cast<Type::Array *>(ref_type->get_sub_type())) {
					auto res = lhs.pointer_subscript(g, rhs.deref(g));
					return preserve_ref? res: res.deref(g);
				}
			if (g.is_const_eval()) {
				if (auto array_type = dynamic_cast<Type::Array *>(lhs.get_type())) {
					auto array = llvm::dyn_cast<llvm::ConstantArray>(lhs.get_llvm_value());
					auto res = Value(
						array_type->get_element_type(),
						array->getAggregateElement(llvm::dyn_cast<llvm::Constant>(rhs.get_llvm_value()))
					);
					return preserve_ref? res: res.deref(g);
				}
				not_const_evaluated(g, this);
			}
			if (dynamic_cast<Type::Pointer *>(lhs.get_type())) {
				auto res = lhs.pointer_subscript(g, rhs.deref(g));
				return preserve_ref? res: res.deref(g);
			} else g.error("Attempt to subscript unknown type: {}", lhs.get_type()->to_string());
		}
		default:
			return bin_op_arithmetic_codegen(g, lhs, rhs, op);
	}
}

Value VarDeclNode::codegen(Codegen &g) const {
	// TODO const eval here?
	// TODO val & var reference
	auto cast_if_needed = [&](Value value) {
		if (type_node) {
			auto type = type_node->codegen(g).get_type_value();
			if (value.get_type() != type)
				g.error(
					"Cannot initialize a variable of type {} with a value of type {}",
					type->to_string(), value.get_type()->to_string()
				);
		}
		return value;
	};
	// TODO check this
	if (inline_flag) {
		Ref::Memory *ref;
		if (value_node) {
			g.push_const_eval();
			auto init = cast_if_needed(value_node->codegen(g));
			if (init.is_ref_value()) {
				g.declare_value(name, init);
				return g.get_context().get_void();
			}
			g.pop_const_eval();
			ref = g.create_ref<Ref::Memory>(g.get_context().get_ref_type(init.get_type(), mutable_flag));
			ref->store(g, init);
		} else {
			auto type = type_node->codegen(g).get_type_value();
			if (dynamic_cast<rin::Type::Ref *>(type))
				g.error("Variable of reference type must be initialized at declaration");
			ref = g.create_ref<Ref::Memory>(g.get_context().get_ref_type(type, mutable_flag));
		}
		g.declare_value(name, Value(ref));
		return g.get_context().get_void();
	}
	Value ptr =
		value_node?
		({
			auto value = cast_if_needed(value_node->codegen(g));
			if (value.is_ref_value()) {
				g.declare_value(name, value);
				return g.get_context().get_void();
			}
			g.allocate_stack(value.get_type(), value, !mutable_flag);
		}): ({
			auto type = type_node->codegen(g).get_type_value();
			if (dynamic_cast<rin::Type::Ref *>(type))
				g.error("Variable of reference type must be initialized at declaration");
			g.allocate_stack(type, !mutable_flag);
		});
	g.declare_value(name, ptr.pointer_subscript(g));
	return g.get_context().get_void();
}

void GlobalVarDeclNode::declare(Codegen &g) {
	// TODO const
	// TODO cycle
	if (g.is_const_eval()) not_const_evaluated(g, this);
	if (inline_flag) g.error("Inline variable at global scope is not supported yet");
	Type *type;
	if (value_node) {
		g.push_const_eval();
		initial_value = value_node->codegen(g);
		g.pop_const_eval();
		if (type_node) {
			auto target_type = type_node->codegen(g).get_type_value();
			if (initial_value.get_type() != target_type)
				g.error(
					"Cannot initialize a variable of type {} with a value of type {}",
					target_type->to_string(), initial_value.get_type()->to_string()
				);
		}
		type = initial_value.get_type();
	} else {
		type = type_node->codegen(g).get_type_value();
		initial_value = Value::undef(type);
	}
	global_ref = g.create_ref_value(
		g.get_context().get_ref_type(type, mutable_flag),
		new llvm::GlobalVariable(
			*g.get_module(),
			type->get_llvm(),
			false /* TODO */,
			llvm::GlobalVariable::LinkageTypes::ExternalLinkage /* TODO */,
			llvm::dyn_cast<llvm::Constant>(initial_value.get_llvm_value())
		)
	);
	g.declare_value(name, global_ref);
}

Value GlobalVarDeclNode::codegen(Codegen &g) const {
	return g.get_context().get_void();
}

Value CallNode::codegen(Codegen &g) const {
	std::optional<Value> receiver;
	if (receiver_node) receiver = receiver_node->codegen(g);
	std::vector<Value> arguments(argument_nodes.size());
	for (size_t i = 0; i < arguments.size(); ++i)
		arguments[i] = argument_nodes[i]->codegen(g);
	if (!g.has_function(name)) // TODO quote needed for error messages?
		g.error("No function named {}", name);
	Function *matching_function = nullptr;
	// TODO error message (where?)
	for (auto &_ : g.lookup_functions(name))
		for (auto &func_ptr : _) {
			auto func = func_ptr->instantiate(g, receiver, arguments);
			if (!func) continue;
			if (!matching_function) matching_function = func;
			else
				g.error(
					"Multiple candidate functions for calling: {} | {}",
					matching_function->get_type_description(),
					func->get_type_description()
				); // TODO at least their names!!
		}
	if (matching_function == nullptr)
		g.error("No matching function for calling");
	if (g.is_const_eval()) {
		if (!matching_function->is_const_eval()) not_const_evaluated(g, this);

	}
	auto res = matching_function->invoke(
		g,
		receiver,
		arguments
	);
	return preserve_ref? res: res.deref(g);
}

Value StructNode::codegen(Codegen &g) const {
	const size_t count = field_names.size();
	std::vector<Type::Struct::FieldInfo> fields;
	fields.reserve(count);
	for (size_t i = 0; i < count; ++i)
		fields.push_back(
			{
				field_names[i],
				field_types[i]->codegen(g).get_type_value()
			}
		);
	return Value(g.get_context().get_struct_type(fields));
}

Value StructValueNode::codegen(Codegen &g) const {
	auto old_type = type_node->codegen(g).get_type_value();
	auto type = dynamic_cast<Type::Struct *>(old_type);
	if (!type) g.error("Expected a struct type, got {}", old_type->to_string());
	auto fields = type->get_fields();
	if (field_nodes.size() != fields.size())
		g.error(
			"Expected {} fields, got {}",
			fields.size(), field_nodes.size()
		);
	// TODO const
	if (g.is_const_eval()) not_const_evaluated(g, this);
	auto ptr = g.allocate_stack(type, true);
	auto &builder = *g.get_builder();
	for (size_t i = 0; i < fields.size(); ++i) {
		auto member = builder.CreateStructGEP(ptr.get_llvm_value(), i);
		auto value = field_nodes[i]->codegen(g);
		if (value.get_type() != fields[i].type)
			g.error(
				"Expected {}, got {} in struct initialization",
				fields[i].type->to_string(), value.get_type()->to_string()
			);
		builder.CreateStore(value.get_llvm_value(), member);
	}
	return g.create_ref_value(
		g.get_context().get_ref_type(type, false),
		ptr.get_llvm_value()
	);
}

Value TupleNode::codegen(Codegen &g) const {
	const size_t count = element_types.size();
	std::vector<Type *> elements(count);
	for (size_t i = 0; i < count; ++i)
		elements[i] = element_types[i]->codegen(g).get_type_value();
	return Value(g.get_context().get_tuple_type(elements));
}

Value TupleValueNode::codegen(Codegen &g) const {
	// TODO const
	const size_t count = element_nodes.size();
	std::vector<Value> elements(count);
	std::vector<Type *> types(count);
	for (size_t i = 0; i < count; ++i) {
		elements[i] = element_nodes[i]->codegen(g);
		types[i] = elements[i].get_type();
	}
	auto type = g.get_context().get_tuple_type(types);
	auto ptr = g.allocate_stack(type, true);
	auto &builder = *g.get_builder();
	for (size_t i = 0; i < elements.size(); ++i) {
		auto member = builder.CreateStructGEP(ptr.get_llvm_value(), i);
		builder.CreateStore(
			elements[i].deref(g).get_llvm_value(),
			member
		);
	}
	return g.create_ref_value(
		g.get_context().get_ref_type(type, false),
		ptr.get_llvm_value()
	);
}

Value BlockNode::codegen(Codegen &g) const {
	Value last = g.get_context().get_void();
	for (const auto &stmt : stmt_nodes) last = stmt->codegen(g);
	return last;
}

Type *FunctionTypeNode::get_receiver_type(Codegen &g) const {
	return receiver_type_node
		   ? receiver_type_node->codegen(g).get_type_value()
		   : nullptr;
}

Type *FunctionTypeNode::get_result_type(Codegen &g) const {
	return result_type_node->codegen(g).get_type_value();
}

std::vector<Value> FunctionTypeNode::get_parameter_types(Codegen &g) const {
	std::vector<Value> result(parameter_type_nodes.size());
	for (size_t i = 0; i < result.size(); ++i)
		result[i] = parameter_type_nodes[i]->codegen(g);
	return result;
}

Value FunctionTypeNode::codegen(Codegen &g) const {
	if (!template_parameters.empty()) // TODO or abstract?
		return g.get_context().get_void();
	std::vector<Type *> parameter_types;
	parameter_types.reserve(parameter_type_nodes.size());
	for (const auto &param : parameter_type_nodes) {
		auto type = param->codegen(g).deref(g);
		parameter_types.push_back(type.get_type_value());
	}
	return Value(g.get_context().get_function_type(
		get_receiver_type(g),
		get_result_type(g),
		parameter_types
	));
}

Value ReturnNode::codegen(Codegen &g) const {
	auto func_type = g.get_function()->get_type();
	auto result_type = func_type->get_result_type();
	// TODO function name in error message
	if (result_type == g.get_context().get_void_type()) {
		if (value_node)
			g.error("Returning a value in a void function: {}", func_type->to_string());
		g.get_builder()->CreateRetVoid();
	} else {
		auto value = value_node->codegen(g);
		if (value.get_type() != result_type)
			g.error(
				"Returning a {} in a function that returns {}",
				value.get_type()->to_string(), result_type->to_string()
			);
		g.get_builder()->CreateRet(value.get_llvm_value());
	}
	return g.get_context().get_void();
}

void FunctionNode::declare(Codegen &g) {
	auto result = type_node->codegen(g);
	if (result.get_type() == g.get_context().get_void_type()) {
		std::vector<std::pair<std::string, Concept *>> concepts;
		std::set<std::string> occurred_strings;
		for (auto &[name, node] : type_node->get_template_parameters()) {
			Concept *concept_value;
			if (node) {
				auto value = node->codegen(g);
				if (!value.is_concept_value())
					g.error("Expected a concept value"); // TODO message
				concept_value = value.get_concept_value();
			} else concept_value = g.get_context().get_any_concept();
			if (!occurred_strings.insert(name).second)
				g.error("Duplicated name in template parameters: {}", name);
			concepts.emplace_back(name, concept_value);
		}
		g.declare_function(
			name,
			std::make_unique<Function::Template>(
				name,
				concepts,
				type_node->get_receiver_type_node(),
				type_node->get_result_type_node(),
				type_node->get_parameter_type_nodes(),
				type_node->get_parameter_names(),
				std::move(content_node),
				inline_flag
			)
		);
		return;
	}
	function_object = g.declare_function(
		dynamic_cast<Type::Function *>(type_node->codegen(g).get_type_value()),
		name,
		inline_flag
	);
	if (!function_object)
		g.error("Redefinition of {} with same parameter types", name); // TODO detailed message
}

Value FunctionNode::codegen(Codegen &g) const {
	if (function_object)
		g.implement_function(
			function_object,
			type_node->get_parameter_names(),
			content_node.get()
		);
	return g.get_context().get_void();
}

Value IfNode::codegen(Codegen &g) const {
	auto &builder = *g.get_builder();
	if (g.is_const_eval()) {
		auto cond_value = condition_node->codegen(g);
		assert(cond_value.is_constant() && cond_value.get_type() == g.get_context().get_boolean_type());
		// TODO all one or?
		const bool cond = llvm::dyn_cast<llvm::ConstantInt>(cond_value.get_llvm_value())->isAllOnesValue();
		if (!else_node) {
			if (cond) then_node->codegen(g);
			return g.get_context().get_void();
		}
		// TODO type inference without generating any code
		const auto
			then_block = g.create_basic_block("then"),
			else_block = g.create_basic_block("else");
		const auto then_ret = ({
			builder.SetInsertPoint(then_block);
			then_node->codegen(g);
		}), else_ret = ({
			builder.SetInsertPoint(else_block);
			else_node->codegen(g);
		});
		auto then_type = then_ret.get_type(), else_type = else_ret.get_type();
		const auto merge_block = g.create_basic_block("merge");
		auto type = then_type == else_type? then_type: nullptr;
		(cond? else_block: then_block)->deleteValue();
		builder.SetInsertPoint(cond? then_block: else_block);
		if (!(cond? then_node: else_node)->has_return()) builder.CreateBr(merge_block);
		builder.SetInsertPoint(merge_block);
		return type? (cond? then_ret: else_ret): g.get_context().get_void();
	}
	const auto
		then_block = g.create_basic_block("then"),
		else_block = g.create_basic_block("else");
	// TODO assert here the value must be boolean? lot of similar code should be modified
	builder.CreateCondBr(
		condition_node->codegen(g).get_llvm_value(),
		then_block,
		else_block
	);
	if (!else_node) {
		builder.SetInsertPoint(then_block);
		then_node->codegen(g);
		if (!then_node->has_return())
			builder.CreateBr(else_block);
		builder.SetInsertPoint(else_block);
		return g.get_context().get_void();
	} else {
		const auto then_ret = ({
			builder.SetInsertPoint(then_block);
			then_node->codegen(g);
		}), else_ret = ({
			builder.SetInsertPoint(else_block);
			else_node->codegen(g);
		});
		auto then_type = then_ret.get_type(), else_type = else_ret.get_type();
		if (then_type == else_type) {
			auto type = then_type;
			auto var = g.allocate_stack(type, false);
			var = {
				dynamic_cast<Type::Ref *>(type)
				? type
				: g.get_context().get_ref_type(type),
				var.get_llvm_value()
			};

			const auto merge_block = g.create_basic_block("merge");

			builder.SetInsertPoint(then_block);
			builder.CreateStore(then_ret.get_llvm_value(), var.get_llvm_value());
			if (!then_node->has_return()) builder.CreateBr(merge_block);
			builder.SetInsertPoint(else_block);
			builder.CreateStore(else_ret.get_llvm_value(), var.get_llvm_value());
			if (!else_node->has_return()) builder.CreateBr(merge_block);

			builder.SetInsertPoint(merge_block);
			return var;
		} else {
			if (!then_node->has_return() || !else_node->has_return()) {
				const auto merge_block = g.create_basic_block("merge");

				builder.SetInsertPoint(then_block);
				if (!then_node->has_return()) builder.CreateBr(merge_block);
				builder.SetInsertPoint(else_block);
				if (!else_node->has_return()) builder.CreateBr(merge_block);

				builder.SetInsertPoint(merge_block);
			}
			return g.get_context().get_void();
		}
	}
}

Value TopLevelNode::codegen(Codegen &g) const {
	for (auto &decl : child_nodes) decl->declare(g);
	for (auto &decl : child_nodes) decl->codegen(g);
	return g.get_context().get_void();
}

} // namespace rin
