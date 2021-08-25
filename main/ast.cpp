
#include "ast.h"
#include "parser.h"

namespace rin {

using K = TokenKind;

[[noreturn]] void not_const_evaluated(Codegen &g, const ASTNode *node) {
	// TODO wtf is this
	g.error(
		"Part of the code ({} ~ {}) is not const evaluated",
		node->get_source_range().begin, node->get_source_range().end
	);
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
	bool is_const
):
	ASTNode(range), name(std::move(name)),
	type_node(std::move(type_node)),
	value_node(std::move(value_node)),
	mutable_flag(is_mutable),
	const_flag(is_const) {
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
	return *opt;
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
	auto value = value_node->codegen(g);
	// TODO remove this
	assert(!g.is_const_eval() || value.is_constant());
	// TODO const pointer/reference
	// TODO array type
	switch (op) {
		case K::Pointer:
			if (value.is_type_value())
				return Value(g.get_context().get_pointer_type(value.get_type_value()));
			else {
				auto v = value.deref(g);
				if (auto ptr_type = dynamic_cast<Type::Pointer *>(v.get_type()))
					return {
						g.get_context().get_ref_type(ptr_type->get_sub_type(), ptr_type->is_const()),
						v.get_llvm_value()
					};
				else unary_op_fail(value, op);
			}
			break;
		case K::Ref:
			if (value.is_type_value())
				return Value(g.get_context().get_ref_type(value.get_type_value()));
			else if (auto ref_type = dynamic_cast<Type::Ref *>(value.get_type()))
				return {
					g.get_context().get_pointer_type(ref_type->get_sub_type(), ref_type->is_const()),
					value.get_llvm_value()
				};
			else unary_op_fail(value, op);
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
	auto ref_type = dynamic_cast<Type::Ref *>(lhs.get_type());
	if (!ref_type)
		g.error(
			"The left side of assignment statement must be a reference, got {}",
			lhs.get_type()->to_string()
		);
	if (ref_type->is_const())
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
		: bin_op_arithmetic_codegen(g, lhs.deref(g), rhs, bop);
	assert(value.get_type() == ref_type->get_sub_type());
	g.get_builder()->CreateStore(
		value.get_llvm_value(),
		lhs.get_llvm_value()
	);
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
		return {
			g.get_context().get_ref_type(struct_type->get_fields()[index].type, ref_type->is_const()),
			g.get_builder()->CreateStructGEP(lhs.get_llvm_value(), index)
		};
	}
	auto lhs = lhs_node->codegen(g);
	if (op == K::LBracket)
		if (auto ref_type = dynamic_cast<Type::Ref *>(lhs.get_type()))
			if (auto tuple_type = dynamic_cast<Type::Tuple *>(ref_type->get_sub_type())) {
				g.push_const_eval();
				auto rhs = rhs_node->codegen(g);
				g.pop_const_eval();
				auto constant = llvm::dyn_cast<llvm::ConstantInt>(rhs.get_llvm_value());
				auto index = constant->getZExtValue();
				return {
					g.get_context().get_ref_type(tuple_type->get_element_types()[index], ref_type->is_const()),
					g.get_builder()->CreateStructGEP(lhs.get_llvm_value(), index)
				};
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
			if (auto ref_type = dynamic_cast<Type::Ref *>(lhs.get_type()))
				if (dynamic_cast<Type::Array *>(ref_type->get_sub_type()))
					return lhs.pointer_subscript(g, rhs.deref(g));
			if (g.is_const_eval()) {
				if (auto array_type = dynamic_cast<Type::Array *>(lhs.get_type())) {
					auto array = llvm::dyn_cast<llvm::ConstantArray>(lhs.get_llvm_value());
					return {
						array_type->get_element_type(),
						array->getAggregateElement(llvm::dyn_cast<llvm::Constant>(rhs.get_llvm_value()))
					};
				}
				not_const_evaluated(g, this);
			}
			if (dynamic_cast<Type::Pointer *>(lhs.get_type())) return lhs.pointer_subscript(g, rhs.deref(g));
			else g.error("Attempt to subscript unknown type: {}", lhs.get_type()->to_string());
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
			if (auto opt = value.cast_to(g, type))
				return *opt;
			else
				g.error(
					"Cannot initialize a variable of type {} with a value of type {}",
					type->to_string(), value.get_type()->to_string()
				);
		}
		return value;
	};
	// TODO not complete
	if (const_flag) {
		auto value = cast_if_needed(value_node->codegen(g));
		g.declare_value(name, value);
		return g.get_context().get_void();
	}
	Value ptr;
	if (value_node) {
		auto value = cast_if_needed(value_node->codegen(g));
		if (dynamic_cast<rin::Type::Ref *>(value.get_type())) {
			g.declare_value(name, value);
			return g.get_context().get_void();
		}
		ptr = g.allocate_stack(value.get_type(), value, !mutable_flag);
	} else {
		auto type = type_node->codegen(g).get_type_value();
		if (dynamic_cast<rin::Type::Ref *>(type))
			g.error("Variable of reference type must be initialized at declaration");
		ptr = g.allocate_stack(type, !mutable_flag);
	}
	g.declare_value(name, ptr.pointer_subscript(g));
	return g.get_context().get_void();
}

void GlobalVarDeclNode::declare(Codegen &g) {
	// TODO const
	// TODO cycle
	if (g.is_const_eval()) not_const_evaluated(g, this);
	Type *type;
	if (value_node) {
		g.push_const_eval();
		initial_value = value_node->codegen(g);
		g.pop_const_eval();
		if (type_node) {
			auto target_type = type_node->codegen(g).get_type_value();
			if (auto opt = initial_value.cast_to(g, target_type))
				initial_value = *opt;
			else
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
	global_ref = {
		g.get_context().get_ref_type(type),
		new llvm::GlobalVariable(
			*g.get_module(),
			type->get_llvm(),
			false /* TODO */,
			llvm::GlobalVariable::LinkageTypes::ExternalLinkage /* TODO */,
			llvm::dyn_cast<llvm::Constant>(initial_value.get_llvm_value())
		)
	};
	g.declare_value(name, global_ref);
}

Value GlobalVarDeclNode::codegen(Codegen &g) const {
	return g.get_context().get_void();
}

inline bool can_cast_to(const Value &value, Type *type) {
	if (value.get_type() == type) return true;
	if (auto ref_type = dynamic_cast<Type::Ref *>(value.get_type()))
		return ref_type->get_sub_type() == type;
	return false;
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
		for (auto &func : _) {
			auto function_type = func->get_type();
			// TODO think over it
			auto receiver_type = function_type->get_receiver_type();
			auto parameter_types = function_type->get_parameter_types();
			if ((receiver_type == nullptr) != (receiver_node == nullptr)) continue;
			if (arguments.size() != parameter_types.size()) continue;
			if (receiver_type && !can_cast_to(*receiver, receiver_type)) continue;
			bool arguments_match = true;
			for (size_t i = 0; i < arguments.size(); ++i)
				if (!can_cast_to(arguments[i], parameter_types[i])) {
					arguments_match = false;
					break;
				}
			if (!arguments_match) continue;
			if (matching_function == nullptr) matching_function = func.get();
			else
				g.error(
					"Multiple candidate functions for calling: \n"
					"  - {}\n"
					"  - {}",
					matching_function->get_type()->to_string(name),
					func->get_type()->to_string(name)
				);
		}
	if (matching_function == nullptr)
		g.error("No matching function for calling");
	if (g.is_const_eval() && !matching_function->is_const_eval())
		not_const_evaluated(g, this);
	return matching_function->invoke(g, receiver, arguments);
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
		builder.CreateStore(
			field_nodes[i]->codegen(g).cast_to(g, fields[i].type)->get_llvm_value(),
			member
		);
	}
	return { g.get_context().get_ref_type(type, true), ptr.get_llvm_value() };
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
			elements[i].get_llvm_value(),
			member
		);
	}
	return { g.get_context().get_ref_type(type, true), ptr.get_llvm_value() };
}

Value BlockNode::codegen(Codegen &g) const {
	Value last = g.get_context().get_void();
	for (const auto &stmt : stmt_nodes) last = stmt->codegen(g);
	return last;
}

Value FunctionTypeNode::codegen(Codegen &g) const {
	std::vector<Type *> param_types;
	param_types.reserve(param_type_nodes.size());
	for (const auto &param : param_type_nodes)
		param_types.push_back(param->codegen(g).get_type_value());
	auto receiver_type =
		receiver_type_node
		? receiver_type_node->codegen(g).get_type_value()
		: nullptr;
	return Value(g.get_context().get_function_type(
		receiver_type,
		result_type_node->codegen(g).get_type_value(),
		param_types
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
	} else if (auto opt = value_node->codegen(g).cast_to(g, result_type)) {
		// TODO TBH, are there such cases? a const return statement? wtf does that even mean?
		g.get_builder()->CreateRet(opt->get_llvm_value());
	}
	return g.get_context().get_void();
}

void FunctionNode::declare(Codegen &g) {
	type = dynamic_cast<Type::Function *>(type_node->codegen(g).get_type_value());
	auto llvm = llvm::Function::Create(
		llvm::dyn_cast<llvm::FunctionType>(
			type->get_llvm()
		),
		llvm::Function::ExternalLinkage,
		// TODO mangle
		name,
		g.get_module()
	);
	function_object = g.declare_function(
		name,
		std::make_unique<Function::Static>(Value(type, llvm), false)
	);
}

Value FunctionNode::codegen(Codegen &g) const {
	auto llvm = function_object->get_llvm_value();
	g.add_layer(
		std::make_unique<llvm::IRBuilder<>>(
			llvm::BasicBlock::Create(
				g.get_llvm_context(),
				"entry",
				llvm
			)
		),
		function_object
	);
	std::vector<llvm::Value *> args;
	for (auto &arg : llvm->args())
		args.push_back(&arg);
	auto receiver_type = type->get_receiver_type();
	const bool has_receiver = receiver_type;
	if (receiver_type)
		g.declare_value(
			"this",
			{ receiver_type, args[0] }
		);
	const auto &param_types = type->get_parameter_types();
	const auto &param_names = type_node->get_parameter_names();
	for (size_t i = 0; i < param_types.size(); ++i)
		g.declare_value(param_names[i], { param_types[i], args[i + has_receiver] });
	body_node->codegen(g);
	if (!body_node->has_return()) {
		if (type->get_result_type() == g.get_context().get_void_type())
			g.get_builder()->CreateRetVoid();
		else g.error("Non-void function does not return a value");
	}
	g.pop_layer();
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
		Type *type = nullptr;
		if (then_type->deref() == else_type->deref()
			&& then_type->deref() != g.get_context().get_void_type())
			type = then_type == else_type? then_type: then_type->deref();
		(cond? else_block: then_block)->deleteValue();
		builder.SetInsertPoint(cond? then_block: else_block);
		if (!(cond? then_node: else_node)->has_return()) builder.CreateBr(merge_block);
		builder.SetInsertPoint(merge_block);
		return type? then_ret.cast_to(g, type).value(): g.get_context().get_void();
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
		if (then_type->deref() == else_type->deref()
			&& then_type->deref() != g.get_context().get_void_type()) {
			auto type = then_type == else_type? then_type: then_type->deref();
			auto var = g.allocate_stack(type, false);
			var = {
				dynamic_cast<Type::Ref *>(type)
				? type
				: g.get_context().get_ref_type(type),
				var.get_llvm_value()
			};

			const auto merge_block = g.create_basic_block("merge");

			builder.SetInsertPoint(then_block);
			builder.CreateStore(then_ret.cast_to(g, type).value().get_llvm_value(), var.get_llvm_value());
			if (!then_node->has_return()) builder.CreateBr(merge_block);
			builder.SetInsertPoint(else_block);
			builder.CreateStore(else_ret.cast_to(g, type).value().get_llvm_value(), var.get_llvm_value());
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
