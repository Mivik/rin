
#include "core_context.h"
#include "type.h"

namespace rin {

Type::Void::Void(CoreContext *ctx):
	Type(llvm::Type::getVoidTy(ctx->get_llvm())) {}

Type::Boolean::Boolean(CoreContext *ctx):
	Type(llvm::Type::getInt1Ty(ctx->get_llvm())) {}

Type::Int::Int(CoreContext *ctx, unsigned int bit_width, bool is_signed):
	Type(llvm::Type::getIntNTy(ctx->get_llvm(), bit_width)),
	signed_flag(is_signed) {}

bool Type::Int::operator==(const Type &other) const {
	if (llvm != other.llvm) return false;
	if (auto *int_type = dynamic_cast<const Type::Int*>(&other))
		return signed_flag == int_type->signed_flag;
	return false;
}

std::string Type::Int::to_string() const {
	return "ui"[signed_flag] + std::to_string(get_bit_width());
}

Type::Array::Array(CoreContext *ctx, Type *element_type, uint32_t size):
	Type(llvm::ArrayType::get(element_type->llvm, size)),
	element_type(element_type), size(size) {}

std::string Type::Array::to_string() const {
	return '[' + element_type->to_string() + ", " + std::to_string(size) + ']';
}

Type::Ref::Ref(CoreContext *ctx, Type *sub_type):
	Type(llvm::PointerType::get(sub_type->get_llvm(), 0)),
	sub_type(sub_type) {}

std::string Type::Ref::to_string() const {
	return '&' + sub_type->to_string();
}

} // namespace rin
