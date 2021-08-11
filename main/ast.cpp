
#include "ast.h"

namespace rin {

using K = TokenKind;

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
			assert(!tmp.empty() && tolower(tmp.back()) == 'l');
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
	throw CodegenException("Unknown constant: " + content);
}

Value ValueNode::codegen(Codegen &g) const {
	auto opt = g.lookup_value(name);
	if (!opt.has_value()) throw CodegenException("Use of undeclared value: " + name);
	return *opt;
}

Value UnaryOpNode::codegen(Codegen &g) const {
	auto unary_op_fail = [](const Value &value, K kind) {
		throw CodegenException(
			"Illegal unary operation on "
			+ value.get_type()->to_string()
			+ ": " + token_kind::name(kind));
	};

	auto &builder = *g.get_builder();
	auto value = value_node->codegen(g).deref(g);
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

[[noreturn]] inline void bin_op_fail(const Value &lhs, const Value &rhs, TokenKind kind) {
	throw CodegenException(
		"Illegal binary operation on "
		+ lhs.get_type()->to_string() + " and " + rhs.get_type()->to_string()
		+ ": " + token_kind::name(kind));
}

inline Value bin_op_arithmetic_codegen(Codegen &g, Value lhs, Value rhs, TokenKind op) {
	lhs = lhs.deref(g);
	rhs = rhs.deref(g);

	auto type = lhs.get_type();
	if (rhs.get_type() != type) bin_op_fail(lhs, rhs, op);

	using bin_op = llvm::Instruction::BinaryOps;
	using cmp_op = llvm::CmpInst::Predicate;

	const bool accept_real = op == K::Add || op == K::Sub || op == K::Mul || op == K::Div;
	const bool result_is_real =
		(dynamic_cast<Type::Real *>(lhs.get_type())
		 || dynamic_cast<Type::Real *>(rhs.get_type()));
	if (!accept_real && result_is_real) bin_op_fail(lhs, rhs, op);

	auto real_result_type = dynamic_cast<Type::Real *>(type);
	auto int_result_type = dynamic_cast<Type::Int *>(type);
	const auto bool_type = g.get_context().get_boolean_type();
	auto &builder = *g.get_builder();

	if (!real_result_type && !int_result_type && type != bool_type)
		bin_op_fail(lhs, rhs, op);

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
				throw CodegenException("Illegal binary operation on real types: " + token_kind::name(op));
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
				bin_op_fail(lhs, rhs, op);
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
				bin_op_fail(lhs, rhs, op);
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
		throw CodegenException(
			"The left side of assignment statement must be a reference"
			", got " + lhs.get_type()->to_string()
		);
	if (ref_type->is_const())
		throw CodegenException("Attempt to assign to a const variable");
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
		throw CodegenException(
			"Attempt to assign a " + rhs.get_type()->to_string() +
			" to a variable of type" + ref_type->get_sub_type()->to_string()
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
	auto lhs = lhs_node->codegen(g), rhs = rhs_node->codegen(g);
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
			lhs = lhs.deref(g);
			if (auto *ptr_type = dynamic_cast<Type::Pointer *>(lhs.get_type())) {
				return lhs.pointer_subscript(g, rhs.deref(g));
			} else throw CodegenException("Attempt to subscript unknown type: " + lhs.get_type()->to_string());
		}
		default:
			return bin_op_arithmetic_codegen(g, lhs, rhs, op);
	}
}

inline bool can_cast_to(const Value &value, Type *type) {
	if (value.get_type() == type) return true;
	if (auto ref_type = dynamic_cast<Type::Ref *>(value.get_type()))
		return ref_type->get_sub_type() == type;
	return false;
}

Value CallNode::codegen(Codegen &g) const {
	auto receiver = receiver_node? receiver_node->codegen(g): Value();
	std::vector<Value> arguments(argument_nodes.size());
	for (size_t i = 0; i < arguments.size(); ++i)
		arguments[i] = argument_nodes[i]->codegen(g);
	if (!g.has_function(name)) // TODO quote needed for error messages?
		throw CodegenException("No function named " + name);
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
			if (!can_cast_to(receiver, receiver_type)) continue;
			bool arguments_match = true;
			for (size_t i = 0; i < arguments.size(); ++i)
				if (!can_cast_to(arguments[i], parameter_types[i])) {
					arguments_match = false;
					break;
				}
			if (!arguments_match) continue;
			if (matching_function == nullptr) matching_function = func.get();
			else
				throw CodegenException(
					"Multiple candidate functions for calling: \n"
					"  - " + matching_function->get_type()->to_string(name) +
					"\n  - " + func->get_type()->to_string(name)
				);
		}
	if (matching_function == nullptr)
		throw CodegenException("No matching function for calling");
	return matching_function->invoke(g, receiver, arguments);
}

} // namespace rin
