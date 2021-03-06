
#include "codegen.h"
#include "core_context.h"
#include "type.h"

namespace rin {

Type *Type::deref(CoreContext &ctx) {
	if (auto ref = dynamic_cast<Type::Ref *>(this))
		return ref->get_sub_type();
	return this;
}

Type::Void::Void(CoreContext *ctx):
	Type(llvm::Type::getVoidTy(ctx->get_llvm())) {}

Type::Boolean::Boolean(CoreContext *ctx):
	Type(llvm::Type::getInt1Ty(ctx->get_llvm())) {}

Type::Int::Int(CoreContext *ctx, unsigned int bit_width, bool is_signed):
	Type(llvm::Type::getIntNTy(ctx->get_llvm(), bit_width)),
	signed_flag(is_signed) {}

bool Type::Int::operator==(const Type &other) const {
	if (llvm != other.llvm) return false;
	if (auto *int_type = dynamic_cast<const Type::Int *>(&other))
		return signed_flag == int_type->signed_flag;
	return false;
}

std::string Type::Int::to_string() const {
	return "ui"[signed_flag] + std::to_string(get_bit_width());
}

Type::Array::Array(Type *element_type, uint32_t size):
	Type(llvm::ArrayType::get(element_type->llvm, size)),
	element_type(element_type), size(size) {}

std::string Type::Array::to_string() const {
	return '[' + element_type->to_string() + ", " + std::to_string(size) + ']';
}

Type::Pointer::Pointer(Type *sub_type, bool const_flag):
	Type(llvm::PointerType::get(sub_type->get_llvm(), 0)),
	sub_type(sub_type), const_flag(const_flag) {
	if (dynamic_cast<Type::Ref *>(sub_type))
		throw CodegenException("Cannot create a pointer to a reference: " + sub_type->to_string());
}

std::string Type::Pointer::to_string() const {
	return (const_flag? "*const ": "*") + sub_type->to_string();
}

Type::Ref::Ref(Type *sub_type, bool const_flag):
	Type(llvm::PointerType::get(sub_type->get_llvm(), 0)),
	sub_type(sub_type), const_flag(const_flag) {
	if (dynamic_cast<Type::Ref *>(sub_type))
		throw CodegenException("Cannot create a reference to a reference: " + sub_type->to_string());
}

std::string Type::Ref::to_string() const {
	return (const_flag? "&const ": "&") + sub_type->to_string();
}

inline std::vector<llvm::Type *> make_llvm_param(
	Type *receiver_type,
	const std::vector<Type *> &param_types
) {
	std::vector<llvm::Type *> ret;
	ret.reserve(param_types.size() + (receiver_type != nullptr));
	if (receiver_type) ret.push_back(receiver_type->get_llvm());
	for (auto para : param_types) ret.push_back(para->get_llvm());
	return ret;
}

Type::Function::Function(
	Type *receiver_type,
	Type *result_type,
	const std::vector<Type *> &param_types
):
	Type(llvm::FunctionType::get(
		result_type->get_llvm(),
		make_llvm_param(receiver_type, param_types),
		false
	)), receiver_type(receiver_type), result_type(result_type),
	param_types(param_types) {}

std::string Type::Function::to_string() const {
	std::string ret;
	if (receiver_type) {
		ret += receiver_type->to_string();
		ret += '.';
	}
	ret += '(';
	for (size_t i = 0; i < param_types.size(); ++i) {
		ret += param_types[i]->to_string();
		if (i != param_types.size() - 1) ret += ", ";
	}
	ret += ')';
	if (!dynamic_cast<Type::Void *>(result_type)) {
		ret += " -> ";
		ret += result_type->to_string();
	}
	return ret;
}

Type::Nothing::Nothing(CoreContext *ctx):
	Type(llvm::Type::getVoidTy(ctx->get_llvm())) {}

std::string Type::Nothing::to_string() const { return "Nothing"; }

} // namespace rin
