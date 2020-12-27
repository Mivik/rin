
#include "codegen.h"
#include "core_context.h"
#include "function.h"
#include "value.h"

namespace rin {

CoreContext::CoreContext():
	void_type(this), nothing_type(this), boolean_type(this),
	i8(this, 8, true), i16(this, 16, true), i32(this, 32, true),
	i64(this, 64, true), i128(this, 128, true),
	u8(this, 8, false), u16(this, 16, false), u32(this, 32, false),
	u64(this, 64, false), u128(this, 128, false),
	float_type(llvm::Type::getFloatTy(llvm), "float"),
	double_type(llvm::Type::getDoubleTy(llvm), "double") {}

Type::Int *CoreContext::get_int_type(unsigned int bit_width, bool is_signed) {
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

Type::Array *CoreContext::get_array_type(Type *element_type, uint32_t size) {
	auto &value = array_type_map[{ element_type, size }];
	if (!value) value = new Type::Array(element_type, size);
	return value;
}

Type::Ref *CoreContext::get_ref_type(Type *type, bool const_flag) {
	auto &value = ref_type_map[{ type, const_flag }];
	if (!value) value = new Type::Ref(type, const_flag);
	return value;
}

Type::Pointer *CoreContext::get_pointer_type(Type *type, bool const_flag) {
	auto &value = pointer_type_map[{ type, const_flag }];
	if (!value) value = new Type::Pointer(type, const_flag);
	return value;
}

Type::Function *CoreContext::get_function_type(
	Type *receiver_type, Type *result_type,
	const std::vector<Type *> &param_types
) {
	auto &value = function_type_map[{{ receiver_type, result_type }, param_types }];
	if (!value) value = new Type::Function(receiver_type, result_type, param_types);
	return value;
}

Value CoreContext::get_void() {
	return Value::undef(get_void_type());
}

Value CoreContext::get_nothing() {
	return Value::undef(get_nothing_type());
}

Function *CoreContext::create_function(
	const std::string &name,
	const Value &value
) {
	const auto pair = std::make_pair(name, dynamic_cast<Type::Function *>(value.get_type()));
	if (function_map.count(pair))
		throw CodegenException(
			"Re-declaring function: " + name
			+ " (" + value.get_type()->to_string() + ')'
		);
	const auto func =
		function_map[pair] = new Function::Static(value);
	function_index[name].push_back(func);
	return func;
}

const std::vector<Function *> &CoreContext::lookup_functions(
	const std::string &name
) {
	static std::vector<Function *> empty;

	auto iter = function_index.find(name);
	if (iter == function_index.end()) return empty;
	return iter->second;
}

CoreContext::~CoreContext() {
	for (auto &[_, value] : int_type_map)
		delete value;
	for (auto &[_, value] : array_type_map)
		delete value;
	for (auto &[_, value] : ref_type_map)
		delete value;
	for (auto &[_, value] : pointer_type_map)
		delete value;
	for (auto &[_, value] : function_type_map)
		delete value;
	for (auto &[_, value] : function_map)
		delete value;
}

} // namespace rin
