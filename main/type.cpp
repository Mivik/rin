
#include "core_context.h"
#include "type.h"

namespace rin {

Type::Int::Int(CoreContext *ctx, unsigned int bit_width, bool is_signed):
	Type(llvm::Type::getIntNTy(ctx->get_llvm(), bit_width)),
	signed_flag(is_signed) {}

bool Type::Int::operator==(const Type &other) const {
	if (llvm != other.llvm) return false;
	if (auto *int_type = dynamic_cast<const Type::Int*>(&other))
		return signed_flag == int_type->signed_flag;
	return false;
}

Type::Array::Array(CoreContext *ctx, Type *element_type, uint32_t size):
	Type(llvm::ArrayType::get(element_type->llvm, size)),
	element_type(element_type), size(size) {}

} // namespace rin
