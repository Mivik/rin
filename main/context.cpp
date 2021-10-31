
#include "context.h"
#include "value.h"

namespace rin {

Context::Context():
	void_type(this), boolean_type(this),
	i8(this, 8, true), i16(this, 16, true),
	i32(this, 32, true), i64(this, 64, true),
	i128(this, 128, true),
	u8(this, 8, false), u16(this, 16, false),
	u32(this, 32, false), u64(this, 64, false),
	u128(this, 128, false),
	float_type(llvm::Type::getFloatTy(llvm), "float"),
	double_type(llvm::Type::getDoubleTy(llvm), "double") {}

Type::Int *Context::get_int_type(unsigned int bit_width, bool is_signed) {
	switch (bit_width) {
		case 8:
			return &(is_signed? i8: u8);
		case 16:
			return &(is_signed? i16: u16);
		case 32:
			return &(is_signed? i32: u32);
		case 64:
			return &(is_signed? i64: u64);
		case 128:
			return &(is_signed? i128: u128);
		default:
			break;
	}
	auto &value = int_type_map[{ bit_width, is_signed }];
	if (!value) value = new Type::Int(this, bit_width, is_signed);
	return value;
}

Type::Array *Context::get_array_type(Type *element_type, uint32_t size) {
	auto &value = array_type_map[{ element_type, size }];
	if (!value) value = new Type::Array(element_type, size);
	return value;
}

Type::Ref *Context::get_ref_type(Type *type, bool mutable_flag) {
	auto &value = ref_type_map[{ type, mutable_flag }];
	if (!value) value = new Type::Ref(type, mutable_flag);
	return value;
}

Type::Pointer *Context::get_pointer_type(Type *type, bool mutable_flag) {
	auto &value = pointer_type_map[{ type, mutable_flag }];
	if (!value) value = new Type::Pointer(type, mutable_flag);
	return value;
}

Type::Struct *Context::get_struct_type(const std::vector<Type::Struct::FieldInfo> &fields) {
	auto &value = struct_type_map[fields];
	if (!value) value = new Type::Struct(this, fields);
	return value;
}

Type::Tuple * Context::get_tuple_type(const std::vector<Type *> &types) {
	auto &value = tuple_type_map[types];
	if (!value) value = new Type::Tuple(this, types);
	return value;
}

Type::Function *Context::get_function_type(
	Type *receiver_type, Type *result_type,
	const std::vector<Type *> &param_types
) {
	auto &value = function_type_map[{{ receiver_type, result_type }, param_types }];
	if (!value) value = new Type::Function(receiver_type, result_type, param_types);
	return value;
}

Value Context::get_void() {
	return Value::undef(get_void_type());
}

Context::~Context() {
	for (auto &[_, value] : int_type_map)
		delete value;
	for (auto &[_, value] : array_type_map)
		delete value;
	for (auto &[_, value] : ref_type_map)
		delete value;
	for (auto &[_, value] : pointer_type_map)
		delete value;
	for (auto &[_, value] : struct_type_map)
		delete value;
	for (auto &[_, value] : tuple_type_map)
		delete value;
	for (auto &[_, value] : function_type_map)
		delete value;
}

} // namespace rin
