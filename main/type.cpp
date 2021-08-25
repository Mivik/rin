
#include "context.h"
#include "type.h"

namespace rin {

Type *Type::common_type(Type *A, Type *B) {
	if (A == B) return A;
	if (auto A_ref = dynamic_cast<Type::Ref *>(A)) {
		if (A_ref->get_sub_type() == B) return B;
		if (auto B_ref = dynamic_cast<Type::Ref *>(B))
			if (A_ref->get_sub_type() == B_ref->get_sub_type())
				return A_ref->is_const()? A_ref: B_ref;
	}
	if (auto B_ref = dynamic_cast<Type::Ref *>(B))
		if (B_ref->get_sub_type() == A) return A;
	return nullptr;
}

Type *Type::deref() {
	if (auto ref = dynamic_cast<Type::Ref *>(this))
		return ref->get_sub_type();
	return this;
}

Type::Void::Void(Context *ctx):
	Type(llvm::Type::getVoidTy(ctx->get_llvm()), false) {}

Type::Boolean::Boolean(Context *ctx):
	Type(llvm::Type::getInt1Ty(ctx->get_llvm()), false) {}

Type::Int::Int(Context *ctx, unsigned int bit_width, bool is_signed):
	Type(llvm::Type::getIntNTy(ctx->get_llvm(), bit_width), false),
	signed_flag(is_signed) {}

bool Type::Int::operator==(const Type &other) const {
	if (llvm != other.llvm) return false;
	if (auto *int_type = dynamic_cast<const Type::Int *>(&other))
		return signed_flag == int_type->signed_flag;
	return false;
}

Type::Array::Array(Type *element_type, uint32_t size):
	Type(llvm::ArrayType::get(element_type->llvm, size), element_type->is_abstract()),
	element_type(element_type), size(size) {}

Type::Pointer::Pointer(Type *sub_type, bool const_flag):
	Type(llvm::PointerType::get(sub_type->get_llvm(), 0), sub_type->is_abstract()),
	sub_type(sub_type), const_flag(const_flag) {
	if (dynamic_cast<Type::Ref *>(sub_type))
		throw std::runtime_error("Cannot create a pointer to a reference: " + sub_type->to_string());
}

Type::Ref::Ref(Type *sub_type, bool const_flag):
	Type(llvm::PointerType::get(sub_type->get_llvm(), 0), sub_type->is_abstract()),
	sub_type(sub_type), const_flag(const_flag) {
	if (dynamic_cast<Type::Ref *>(sub_type))
		throw std::runtime_error("Cannot create a reference to a reference: " + sub_type->to_string());
}

inline llvm::Type *make_struct_type(
	Context *ctx,
	const std::vector<Type::Struct::FieldInfo> &fields
) {
	llvm::OwningArrayRef<llvm::Type *> arr(fields.size());
	for (size_t i = 0; i < arr.size(); ++i)
		arr[i] = fields[i].type->get_llvm();
	return llvm::StructType::create(ctx->get_llvm(), arr);
}

Type::Struct::Struct(Context *ctx, std::vector<FieldInfo> fields):
	Type(
		make_struct_type(ctx, fields),
		std::any_of(fields.begin(), fields.end(), [](const FieldInfo &f) {
			return f.type->is_abstract();
		})
	),
	fields(std::move(fields)) {
	assert(!this->fields.empty());
}

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

inline llvm::Type *make_tuple_type(
	Context *ctx,
	const std::vector<Type *> &elements
) {
	llvm::OwningArrayRef<llvm::Type *> arr(elements.size());
	for (size_t i = 0; i < arr.size(); ++i) arr[i] = elements[i]->get_llvm();
	return llvm::StructType::create(ctx->get_llvm(), arr);
}

Type::Tuple::Tuple(Context *ctx, std::vector<Type *> elements):
	Type(
		make_tuple_type(ctx, elements),
		std::any_of(elements.begin(), elements.end(), [](const Type *type) {
			return type->is_abstract();
		})
	),
	elements(std::move(elements)) {
	assert(this->elements.size() > 1);
}

std::string Type::Tuple::to_string() const {
	std::string res = "(";
	for (size_t i = 0; i < elements.size(); ++i) {
		res += elements[i]->to_string();
		if (i != elements.size() - 1) res += ", ";
	}
	res += ')';
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
	Type(
		result_type->get_llvm()? llvm::FunctionType::get(
			result_type->get_llvm(),
			make_llvm_param(receiver_type, param_types),
			false
		): nullptr,
		(receiver_type && receiver_type->is_abstract()) ||
		(result_type && result_type->is_abstract()) ||
		std::any_of(param_types.begin(), param_types.end(), [](const Type *type) {
			return type->is_abstract();
		})
	),
	receiver_type(receiver_type),
	result_type(result_type),
	param_types(param_types) {}

std::string Type::Function::to_string(const std::string &name) const {
	std::string ret = "fn ";
	if (receiver_type) {
		ret += '[';
		ret += receiver_type->to_string();
		ret += ']';
		ret += '.';
	}
	ret += name;
	ret += '(';
	for (size_t i = 0; i < param_types.size(); ++i) {
		ret += param_types[i]->to_string();
		if (i != param_types.size() - 1) ret += ", ";
	}
	ret += ')';
	if (!dynamic_cast<Type::Void *>(result_type)) {
		ret += ": ";
		ret += result_type->to_string();
	}
	return ret;
}

} // namespace rin
