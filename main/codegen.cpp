
#include <cctype>

#include <llvm/ADT/APInt.h>

#include "ast.h"
#include "codegen.h"
#include "utility.h"

namespace rin {

Value ConstantNode::codegen(Context &ctx) const {
	auto &core = ctx.get_core();
	if (str == "true" || str == "false") {
		auto type = core.get_boolean_type();
		return {
				type,
				ptr_cast<llvm::Value>(llvm::ConstantInt::get(type->get_llvm(), str == "true"))
			};
	}
	if (isdigit(str[0])) {
		auto tmp = str; assert(!tmp.empty());
		auto type = core.get_i32_type();
		if (tolower(tmp.back()) == 'l') {
			tmp.pop_back(); assert(!tmp.empty() && tolower(tmp.back()) == 'l');
			tmp.pop_back(); assert(!tmp.empty());
			if (tolower(tmp.back()) == 'u') {
				tmp.pop_back();
				type = core.get_u64_type();
			} else type = core.get_i64_type();
		} else if (tolower(tmp.back()) == 'u') {
			tmp.pop_back(); assert(!tmp.empty());
			type = core.get_u32_type();
		}
		return { type, ptr_cast<llvm::Value>(
					llvm::ConstantInt::get(
						ptr_cast<llvm::IntegerType>(type->get_llvm()),
						tmp, 10
					)
				) };
	}
	rin_unreachable("Unknown constant: " + str);
}

[[noreturn]] inline void unary_op_fail(const Value &value, TokenKind op) {
	throw CodegenException(
			"Illegal unary operation on "
			+ value.get_type()->to_string()
			+ ": " + token_kind::name(op));
}

Value UnaryOpNode::codegen(Context &ctx) const {
	auto &builder = ctx.get_builder();
	auto value = value_node->codegen(ctx);
	auto type = value.get_type();
	// TODO Logical operators!!
	if (dynamic_cast<Type::Real*>(type)) {
		switch (op) {
			case UAdd: return value;
			case USub: return { type, builder.CreateFNeg(value.get_llvm()) };
			default: unary_op_fail(value, op);
		}
	} else if (dynamic_cast<Type::Int*>(value.get_type())) {
		switch (op) {
			case UAdd: return value;
			case USub: return { type, builder.CreateNeg(value.get_llvm()) };
			case Not: return { type, builder.CreateNot(value.get_llvm()) };
			default: unary_op_fail(value, op);
		}
	} else unary_op_fail(value, op);
}

[[noreturn]] inline void bin_op_fail(const Value &lhs, const Value &rhs, TokenKind op) {
	throw CodegenException(
			"Illegal binary operation on "
			+ lhs.get_type()->to_string() + " and " + rhs.get_type()->to_string()
			+ ": " + token_kind::name(op));
}

inline Type* bin_op_arithmetic_result_type(Type *lhs, Type *rhs) {
	const bool lhs_is_real = dynamic_cast<Type::Real*>(lhs);
	const bool rhs_is_real = dynamic_cast<Type::Real*>(rhs);
	if (lhs_is_real != rhs_is_real) return lhs_is_real? lhs: rhs;
	const auto lhs_size = lhs->scalar_size_in_bits(), rhs_size = rhs->scalar_size_in_bits();
	if (lhs_size != rhs_size)
		return (lhs_size > rhs_size)? lhs: rhs;
	if (auto lhs_int_type = dynamic_cast<Type::Int*>(lhs))
		return lhs_int_type->is_signed()? lhs: rhs;
	return lhs;
}

inline Value bin_op_arithmetic_codegen(Context &ctx, Value lhs, Value rhs, TokenKind op) {
	using bin_op = llvm::Instruction::BinaryOps;

	const bool accept_real = op == Add || op == Sub || op == Mul || op == Div;
	const bool result_is_real =
		(dynamic_cast<Type::Real*>(lhs.get_type())
		|| dynamic_cast<Type::Real*>(rhs.get_type()));
	if (!accept_real && result_is_real)
		throw CodegenException(
			"Illegal operation on "
			+ lhs.get_type()->to_string() + " and "
			+ rhs.get_type()->to_string() + ": " + token_kind::name(op));

	auto origin_lhs_type = lhs.get_type();
	auto result_type = bin_op_arithmetic_result_type(lhs.get_type(), rhs.get_type());
	lhs = lhs.cast(ctx, result_type);
	rhs = rhs.cast(ctx, result_type);

	bin_op llvm_op;
	// TODO Logical binary operators
	if (dynamic_cast<Type::Real*>(result_type)) {
		switch (op) {
			case Add: llvm_op = bin_op::FAdd; break;
			case Sub: llvm_op = bin_op::FSub; break;
			case Mul: llvm_op = bin_op::FMul; break;
			case Div: llvm_op = bin_op::FDiv; break;
			default: throw CodegenException("Illegal binary operation on real types: " + token_kind::name(op));
		}
	} else if (auto int_type = dynamic_cast<Type::Int*>(result_type)) {
		switch (op) {
			case Add: llvm_op = bin_op::Add; break;
			case Sub: llvm_op = bin_op::Sub; break;
			case Mul: llvm_op = bin_op::Mul; break;
			case Xor: llvm_op = bin_op::Xor; break;
			case Or: llvm_op = bin_op::Or; break;
			case And: llvm_op = bin_op::And; break;
			case Shl: result_type = origin_lhs_type; llvm_op = bin_op::Shl; break;
			case Shr:
				result_type = origin_lhs_type;
				llvm_op = ptr_cast<Type::Int>(result_type)->is_signed()? bin_op::AShr: bin_op::LShr;
				break;
			case Div: llvm_op = int_type->is_signed()? bin_op::SDiv: bin_op::UDiv; break;
			case Mod: llvm_op = int_type->is_signed()? bin_op::SRem: bin_op::URem; break;
			default: bin_op_fail(lhs, rhs, op);
		}
	} else bin_op_fail(lhs, rhs, op);
	return
		{
			result_type,
			ctx.get_builder().CreateBinOp(llvm_op, lhs.get_llvm(), rhs.get_llvm())
		};
}

Value BinOpNode::codegen(Context &ctx) const {
	auto lhs = lhs_node->codegen(ctx), rhs = rhs_node->codegen(ctx);
	switch (op) {
		case Add: case Sub: case Mul: case Div: case Mod:
		case Shl: case Shr: case Or: case And: case Xor:
			return bin_op_arithmetic_codegen(ctx, lhs, rhs, op);
		default: break;
	}
	bin_op_fail(lhs, rhs, op);
}

} // namespace rin
