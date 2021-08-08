
#include "context.h"
#include "type.h"

namespace rin {

Type::Void::Void(Context *ctx):
	Type(llvm::Type::getVoidTy(ctx->get_llvm())) {}

Type::Boolean::Boolean(Context *ctx):
	Type(llvm::Type::getInt1Ty(ctx->get_llvm())) {}

Type::Int::Int(Context *ctx, unsigned int bit_width, bool is_signed):
	Type(llvm::Type::getIntNTy(ctx->get_llvm(), bit_width)),
	signed_flag(is_signed) {}

bool Type::Int::operator==(const Type &other) const {
	if (llvm != other.llvm) return false;
	if (auto *int_type = dynamic_cast<const Type::Int *>(&other))
		return signed_flag == int_type->signed_flag;
	return false;
}

Type::Array::Array(Type *element_type, uint32_t size):
	Type(llvm::ArrayType::get(element_type->llvm, size)),
	element_type(element_type), size(size) {}

Type::Pointer::Pointer(Type *sub_type, bool const_flag):
	Type(llvm::PointerType::get(sub_type->get_llvm(), 0)),
	sub_type(sub_type), const_flag(const_flag) {
	if (dynamic_cast<Type::Ref *>(sub_type))
		throw std::runtime_error("Cannot create a pointer to a reference: " + sub_type->to_string());
}

Type::Ref::Ref(Type *sub_type, bool const_flag):
	Type(llvm::PointerType::get(sub_type->get_llvm(), 0)),
	sub_type(sub_type), const_flag(const_flag) {
	if (dynamic_cast<Type::Ref *>(sub_type))
		throw std::runtime_error("Cannot create a reference to a reference: " + sub_type->to_string());
}

inline llvm::ArrayRef<llvm::Type *> make_llvm_types(const std::vector<Type::Struct::FieldInfo> &fields) {
	llvm::OwningArrayRef<llvm::Type *> ret(fields.size());
	for (size_t i = 0; i < ret.size(); ++i)
		ret[i] = fields[i].type->get_llvm();
	return std::move(ret);
}

Type::Struct::Struct(Context *ctx, std::vector<FieldInfo> fields):
	Type(llvm::StructType::get(ctx->get_llvm(), make_llvm_types(fields))),
	fields(std::move(fields)) {}

std::string Type::Struct::to_string() const {
	std::string res = "{";
	for (size_t i = 0; i < fields.size(); ++i) {
		res += ' ';
		res += fields[i].name;
		res += ": ";
		res += fields[i].type->to_string();
		if (i != fields.size() - 1) res += ',';
	}
	res += " }";
	return res;
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
	)),
	receiver_type(receiver_type),
	result_type(result_type),
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

} // namespace rin