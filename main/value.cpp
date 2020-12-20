
#include "codegen.h"
#include "value.h"

namespace rin {

Value Value::cast(Context &ctx, Type *to, bool implicit_only, bool check_only) const {
	using cast_op = llvm::Instruction::CastOps;

	if (type == to) return *this;
	cast_op llvm_op;
	const bool larger = type->scalar_size_in_bits() > to->scalar_size_in_bits();
	if (dynamic_cast<Type::Real*>(to)) {
		if (auto int_type = dynamic_cast<Type::Int*>(type))
			llvm_op = int_type->is_signed()? cast_op::SIToFP: cast_op::UIToFP;
		else if (dynamic_cast<Type::Real*>(type))
			llvm_op = larger? cast_op::FPTrunc: cast_op::FPExt;
		else throw CastException(type, to);
	} else if (auto to_int_type = dynamic_cast<Type::Int*>(to)) {
		if (auto from_int_type = dynamic_cast<Type::Int*>(type)) {
			if (from_int_type->is_signed())
				llvm_op = larger? cast_op::Trunc: cast_op::SExt;
			else
				llvm_op = larger? cast_op::Trunc: cast_op::ZExt;
		} else if (dynamic_cast<Type::Real*>(type))
			llvm_op = to_int_type->is_signed()? cast_op::FPToSI: cast_op::FPToUI;
		else throw CastException(type, to);
	} else throw CastException(type, to);
	return { to, ctx.get_builder().CreateCast(llvm_op, llvm, to->get_llvm()) };
}

Value Value::deref(Context &ctx) {
	auto ref_type = dynamic_cast<Type::Ref*>(type);
	if (!ref_type) return *this;
	auto sub = ref_type->get_sub_type();
	return {
		sub,
		ctx.get_builder().CreateLoad(sub->get_llvm(), llvm)
	};
}

Value Value::pointer_subscript(Context &ctx) {
	auto ptr_type = dynamic_cast<Type::Pointer*>(type);
	if (!ptr_type)
		throw CodegenException(
			"Subscripting a non-pointer type: " +
			type->to_string()
		);
	return {
		ctx.get_core().get_ref_type(
			ptr_type->get_sub_type(),
			ptr_type->is_const()
		),
		llvm
	};
}

Value Value::pointer_subscript(Context &ctx, Value index) {
	auto ptr_type = dynamic_cast<Type::Pointer*>(type);
	if (!ptr_type)
		throw CodegenException(
			"Subscripting a non-pointer type: " +
			type->to_string()
		);
	return {
		ctx.get_core().get_ref_type(
			ptr_type->get_sub_type(),
			ptr_type->is_const()
		),
		ctx.get_builder().CreateGEP(llvm, index.llvm)
	};
}

} // namespace rin
