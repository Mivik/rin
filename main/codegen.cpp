
#include <cctype>

#include <llvm/ADT/APInt.h>

#include "ast.h"
#include "codegen.h"
#include "function.h"
#include "utility.h"

namespace rin {

Value ConstantNode::codegen(Context &ctx) const {
	auto &core = ctx.get_core();
	if (str == "true" || str == "false") {
		auto type = core.get_boolean_type();
		return {
			type,
			direct_cast<llvm::Value>(llvm::ConstantInt::get(type->get_llvm(), str == "true"))
		};
	}
	if (isdigit(str[0])) {
		auto tmp = str;
		assert(!tmp.empty());
		auto type = core.get_i32_type();
		if (tolower(tmp.back()) == 'l') {
			tmp.pop_back();
			assert(!tmp.empty() && tolower(tmp.back()) == 'l');
			tmp.pop_back();
			assert(!tmp.empty());
			if (tolower(tmp.back()) == 'u') {
				tmp.pop_back();
				type = core.get_u64_type();
			} else type = core.get_i64_type();
		} else if (tolower(tmp.back()) == 'u') {
			tmp.pop_back();
			assert(!tmp.empty());
			type = core.get_u32_type();
		}
		return { type, direct_cast<llvm::Value>(
			llvm::ConstantInt::get(
				direct_cast<llvm::IntegerType>(type->get_llvm()),
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
	if (dynamic_cast<Type::Real *>(type)) {
		switch (op) {
			case UAdd:
				return value;
			case USub:
				return { type, builder.CreateFNeg(value.get_llvm()) };
			default:
				unary_op_fail(value, op);
		}
	} else if (dynamic_cast<Type::Int *>(value.get_type())) {
		switch (op) {
			case UAdd:
				return value;
			case USub:
				return { type, builder.CreateNeg(value.get_llvm()) };
			case Not:
				return { type, builder.CreateNot(value.get_llvm()) };
			default:
				unary_op_fail(value, op);
		}
	} else if (value.get_type() == ctx.get_core().get_boolean_type()) {
		if (op == LNot) return { type, builder.CreateNot(value.get_llvm()) };
		else unary_op_fail(value, op);
	} else unary_op_fail(value, op);
}

[[noreturn]] inline void bin_op_fail(const Value &lhs, const Value &rhs, TokenKind op) {
	throw CodegenException(
		"Illegal binary operation on "
		+ lhs.get_type()->to_string() + " and " + rhs.get_type()->to_string()
		+ ": " + token_kind::name(op));
}

inline Type *bin_op_arithmetic_result_type(Type *lhs, Type *rhs) {
	const bool lhs_is_real = dynamic_cast<Type::Real *>(lhs);
	const bool rhs_is_real = dynamic_cast<Type::Real *>(rhs);
	if (lhs_is_real != rhs_is_real) return lhs_is_real? lhs: rhs;
	const auto lhs_size = lhs->scalar_size_in_bits(), rhs_size = rhs->scalar_size_in_bits();
	if (lhs_size != rhs_size)
		return (lhs_size > rhs_size)? lhs: rhs;
	if (auto lhs_int_type = dynamic_cast<Type::Int *>(lhs))
		return lhs_int_type->is_signed()? lhs: rhs;
	return lhs;
}

inline Value bin_op_arithmetic_codegen(Context &ctx, Value lhs, Value rhs, TokenKind op) {
	lhs = lhs.deref(ctx);
	rhs = rhs.deref(ctx);

	using bin_op = llvm::Instruction::BinaryOps;
	using cmp_op = llvm::CmpInst::Predicate;

	const bool accept_real = op == Add || op == Sub || op == Mul || op == Div;
	const bool result_is_real =
		(dynamic_cast<Type::Real *>(lhs.get_type())
		 || dynamic_cast<Type::Real *>(rhs.get_type()));
	if (!accept_real && result_is_real) bin_op_fail(lhs, rhs, op);

	auto origin_lhs_type = lhs.get_type();
	auto result_type = bin_op_arithmetic_result_type(lhs.get_type(), rhs.get_type());
	lhs = lhs.cast(ctx, result_type);
	rhs = rhs.cast(ctx, result_type);

	auto real_result_type = dynamic_cast<Type::Real *>(result_type);
	auto int_result_type = dynamic_cast<Type::Int *>(result_type);
	const auto bool_type = ctx.get_core().get_boolean_type();
	auto &builder = ctx.get_builder();

	if (!real_result_type && !int_result_type && result_type != bool_type)
		bin_op_fail(lhs, rhs, op);

	cmp_op cmp = cmp_op::FCMP_FALSE;
	auto branch = [&](cmp_op for_signed, cmp_op for_unsigned, cmp_op for_real) {
		if (result_type == bool_type) cmp = for_unsigned;
		else cmp = real_result_type? for_real: (int_result_type->is_signed()? for_signed: for_unsigned);
	};
	switch (op) {
		case Lt:
			branch(cmp_op::ICMP_SLT, cmp_op::ICMP_ULT, cmp_op::FCMP_OLT);
			break;
		case Le:
			branch(cmp_op::ICMP_SLE, cmp_op::ICMP_ULE, cmp_op::FCMP_OLE);
			break;
		case Gt:
			branch(cmp_op::ICMP_SGT, cmp_op::ICMP_UGT, cmp_op::FCMP_OGT);
			break;
		case Ge:
			branch(cmp_op::ICMP_SGE, cmp_op::ICMP_UGE, cmp_op::FCMP_OGE);
			break;
		case Eq:
			branch(cmp_op::ICMP_EQ, cmp_op::ICMP_EQ, cmp_op::FCMP_OEQ);
			break;
		case Neq:
			branch(cmp_op::ICMP_NE, cmp_op::ICMP_NE, cmp_op::FCMP_ONE);
			break;
		default:
			break;
	}
	if (cmp != cmp_op::FCMP_FALSE) {
		if (real_result_type)
			return { bool_type, builder.CreateFCmp(cmp, lhs.get_llvm(), rhs.get_llvm()) };
		else
			return { bool_type, builder.CreateICmp(cmp, lhs.get_llvm(), rhs.get_llvm()) };
	}

	bin_op llvm_op;
	// TODO Logical binary operators
	if (real_result_type) {
		switch (op) {
			case Add:
				llvm_op = bin_op::FAdd;
				break;
			case Sub:
				llvm_op = bin_op::FSub;
				break;
			case Mul:
				llvm_op = bin_op::FMul;
				break;
			case Div:
				llvm_op = bin_op::FDiv;
				break;
			default:
				throw CodegenException("Illegal binary operation on real types: " + token_kind::name(op));
		}
	} else if (int_result_type) {
		switch (op) {
			case Add:
				llvm_op = bin_op::Add;
				break;
			case Sub:
				llvm_op = bin_op::Sub;
				break;
			case Mul:
				llvm_op = bin_op::Mul;
				break;
			case Xor:
				llvm_op = bin_op::Xor;
				break;
			case Or:
				llvm_op = bin_op::Or;
				break;
			case And:
				llvm_op = bin_op::And;
				break;
			case Shl:
				result_type = origin_lhs_type;
				llvm_op = bin_op::Shl;
				break;
			case Shr:
				result_type = origin_lhs_type;
				llvm_op = direct_cast<Type::Int>(result_type)->is_signed()? bin_op::AShr: bin_op::LShr;
				break;
			case Div:
				llvm_op = int_result_type->is_signed()? bin_op::SDiv: bin_op::UDiv;
				break;
			case Mod:
				llvm_op = int_result_type->is_signed()? bin_op::SRem: bin_op::URem;
				break;
			default:
				bin_op_fail(lhs, rhs, op);
		}
	} else bin_op_fail(lhs, rhs, op);
	return
		{
			result_type,
			builder.CreateBinOp(llvm_op, lhs.get_llvm(), rhs.get_llvm())
		};
}

inline Value assignment_codegen(Context &ctx, Value lhs, Value rhs, TokenKind op) {
	auto ref_type = dynamic_cast<Type::Ref *>(lhs.get_type());
	if (!ref_type)
		throw CodegenException(
			"The left side of assignment statement must be a reference"
			", got" + lhs.get_type()->to_string()
		);
	if (ref_type->is_const())
		throw CodegenException("Attempt to assign to a const variable");
	TokenKind bop = Assign;
	switch (op) {
#define H(s) case s##A: bop = s; break;
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
		case Assign:
			break;
		default:
			assert(false);
	}
	rhs = rhs.cast(ctx, ref_type->get_sub_type());
	auto value =
		bop == Assign
		? rhs
		: bin_op_arithmetic_codegen(ctx, lhs.deref(ctx), rhs, bop);
	ctx.get_builder().CreateStore(
		value.cast(ctx, ref_type->get_sub_type()).get_llvm(),
		lhs.get_llvm()
	);
	return lhs;
}

Value BinOpNode::codegen(Context &ctx) const {
	auto lhs = lhs_node->codegen(ctx), rhs = rhs_node->codegen(ctx);
	switch (op) {
		case Add:
		case Sub:
		case Mul:
		case Div:
		case Mod:
		case Shl:
		case Shr:
		case Or:
		case And:
		case Xor:
		case Lt:
		case Le:
		case Gt:
		case Ge:
		case Eq:
		case Neq:
			return bin_op_arithmetic_codegen(ctx, lhs, rhs, op);
		case Assign:
		case AddA:
		case SubA:
		case MulA:
		case DivA:
		case ModA:
		case ShlA:
		case ShrA:
		case OrA:
		case AndA:
		case XorA:
			return assignment_codegen(ctx, lhs, rhs, op);
		default:
			break;
	}
	bin_op_fail(lhs, rhs, op);
}

Value NamedValueNode::codegen(Context &ctx) const {
	auto opt = ctx.lookup_value(name);
	if (!opt) throw CodegenException("Unknown identifier: " + name);
	return *opt;
}

Type *NamedTypeNode::codegen(Context &ctx) const {
	auto opt = ctx.lookup_type(name);
	if (!opt) throw CodegenException("Unknown type: " + name);
	return *opt;
}

Type *ArrayTypeNode::codegen(Context &ctx) const {
	return ctx.get_core().get_array_type(
		element_type_node->codegen(ctx),
		size
	);
}

Type *PointerTypeNode::codegen(Context &ctx) const {
	return ctx.get_core().get_pointer_type(sub_type_node->codegen(ctx), const_flag);
}

Type *RefTypeNode::codegen(Context &ctx) const {
	return ctx.get_core().get_ref_type(sub_type_node->codegen(ctx), const_flag);
}

Type *FunctionTypeNode::codegen(Context &ctx) const {
	std::vector<Type *> param_types;
	param_types.reserve(param_type_nodes.size());
	for (auto param : param_type_nodes)
		param_types.push_back(param->codegen(ctx));
	auto receiver_type =
		receiver_type_node
		? receiver_type_node->codegen(ctx)
		: nullptr;
	return ctx.get_core().get_function_type(
		receiver_type,
		result_type_node->codegen(ctx),
		param_types
	);
}

Value VarDeclNode::codegen(Context &ctx) const {
	Value ptr;
	if (value_node) {
		auto value = value_node->codegen(ctx);
		if (type_node)
			value = value.cast(ctx, type_node->codegen(ctx));
		ptr = ctx.allocate_stack(value.get_type(), value, const_flag);
	} else
		ptr = ctx.allocate_stack(type_node->codegen(ctx), const_flag);
	ctx.declare_value(name, ptr.pointer_subscript(ctx));
	return ctx.get_core().get_nothing();
}

Value BlockNode::codegen(Context &ctx) const {
	Value last = ctx.get_core().get_nothing();
	for (auto stmt : stmts)
		last = stmt->codegen(ctx);
	return last;
}

Prototype PrototypeNode::codegen(Context &ctx) const {
	auto func_type =
		dynamic_cast<Type::Function *>(type_node->codegen(ctx));
	return {
		name,
		func_type,
		param_names
	};
}

void FunctionNode::codegen(Context &ctx) const {
	auto prototype = prototype_node->codegen(ctx);
	auto llvm = llvm::Function::Create(
		llvm::dyn_cast<llvm::FunctionType>(
			prototype.get_function_type()->get_llvm()
		),
		llvm::Function::ExternalLinkage,
		prototype.get_name(),
		ctx.get_module()
	);
	auto func = ctx.get_core().create_function(
		prototype.get_name(),
		{ prototype.get_function_type(), llvm }
	);
	ctx.add_layer(
		std::make_unique<llvm::IRBuilder<>>(
			llvm::BasicBlock::Create(
				ctx.get_llvm(),
				"entry",
				llvm
			)
		),
		func
	);
	std::vector<llvm::Value *> args;
	for (auto &arg : llvm->args())
		args.push_back(&arg);
	auto type = prototype.get_function_type();
	auto receiver_type = type->get_receiver_type();
	const bool has_receiver = receiver_type;
	if (receiver_type)
		ctx.declare_value("this",
						  { ctx.get_core().get_ref_type(receiver_type), args[0] }
		);
	const auto &param_types = type->get_parameter_types();
	const auto &param_names = prototype.get_parameter_names();
	for (size_t i = 0; i < param_types.size(); ++i)
		ctx.declare_value(param_names[i], { param_types[i], args[i + has_receiver] });
	body_node->codegen(ctx);
	ctx.pop_layer();
}

Value ReturnNode::codegen(Context &ctx) const {
	auto result_type = ctx.get_function()->get_type()->get_result_type();
	if (result_type == ctx.get_core().get_void_type()) {
		if (value_node)
			throw CodegenException(
				"Returning a value in a void function: "
				+ value_node->to_string()
			);
		ctx.get_builder().CreateRetVoid();
	} else
		ctx.get_builder().CreateRet(value_node->codegen(ctx).cast(ctx, result_type).get_llvm());
	return ctx.get_core().get_nothing();
}

Value IfNode::codegen(Context &ctx) const {
	auto &builder = ctx.get_builder();
	auto cond = condition_node->codegen(ctx);
	const auto
		then_block = ctx.create_basic_block("then"),
		else_block = ctx.create_basic_block("else");
	builder.CreateCondBr(
		cond.get_llvm(),
		then_block,
		else_block
	);
	if (!else_node) {
		builder.SetInsertPoint(then_block);
		then_node->codegen(ctx);
		if (!then_node->has_return())
			builder.CreateBr(else_block);
		builder.SetInsertPoint(else_block);
		return ctx.get_core().get_nothing();
	} else {
		const auto then_ret = ({
			builder.SetInsertPoint(then_block);
			then_node->codegen(ctx);
		}), else_ret = ({
			builder.SetInsertPoint(else_block);
			else_node->codegen(ctx);
		});
		if (then_ret.get_type() == else_ret.get_type()
			&& then_ret.get_type() != ctx.get_core().get_nothing_type()) {
			auto var = ctx.allocate_stack(then_ret.get_type(), false);
			var = {
				ctx.get_core().get_ref_type(dynamic_cast<Type::Pointer *>(var.get_type())->get_sub_type()),
				var.get_llvm()
			};

			const auto merge_block = ctx.create_basic_block("merge");

			builder.SetInsertPoint(then_block);
			builder.CreateStore(then_ret.get_llvm(), var.get_llvm());
			if (!then_node->has_return()) builder.CreateBr(merge_block);
			builder.SetInsertPoint(else_block);
			builder.CreateStore(else_ret.get_llvm(), var.get_llvm());
			if (!else_node->has_return()) builder.CreateBr(merge_block);

			builder.SetInsertPoint(merge_block);
			return var;
		} else {
			if (!then_node->has_return() || !else_node->has_return()) {
				const auto merge_block = ctx.create_basic_block("merge");

				builder.SetInsertPoint(then_block);
				if (!then_node->has_return()) builder.CreateBr(merge_block);
				builder.SetInsertPoint(else_block);
				if (!else_node->has_return()) builder.CreateBr(merge_block);

				builder.SetInsertPoint(merge_block);
			}
			return ctx.get_core().get_nothing();
		}
	}
}

} // namespace rin
