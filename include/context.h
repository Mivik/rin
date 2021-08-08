
#pragma once

#include <unordered_map>

#include <llvm/IR/LLVMContext.h>

#include "type.h"
#include "utility.h"

namespace rin {

class Value;

class Context {
public:
	Context();
	~Context();

	llvm::LLVMContext &get_llvm() { return llvm; }
	Type::Void *get_void_type() { return &void_type; }
	Type::Boolean *get_boolean_type() { return &boolean_type; }
	Type::Int *get_i8_type() { return &i8; }
	Type::Int *get_i16_type() { return &i16; }
	Type::Int *get_i32_type() { return &i32; }
	Type::Int *get_i64_type() { return &i64; }
	Type::Int *get_i128_type() { return &i128; }
	Type::Int *get_u8_type() { return &u8; }
	Type::Int *get_u16_type() { return &u16; }
	Type::Int *get_u32_type() { return &u32; }
	Type::Int *get_u64_type() { return &u64; }
	Type::Int *get_u128_type() { return &u128; }
	Type::Real *get_float_type() { return &float_type; }
	Type::Real *get_double_type() { return &double_type; }
	Type::Int *get_int_type(unsigned int bit_width, bool is_signed = true);
	Type::Array *get_array_type(Type *element_type, uint32_t size);
	Type::Ref *get_ref_type(Type *type, bool const_flag = false);
	Type::Pointer *get_pointer_type(Type *type, bool const_flag = false);
	Type::Struct *get_struct_type(const std::vector<Type::Struct::FieldInfo> &fields);
	Type::Function *get_function_type(
		Type *receiver_type, Type *result_type,
		const std::vector<Type *> &param_types
	);

	DISABLE_COPY(Context)

private:
	llvm::LLVMContext llvm;
	Type::Void void_type;
	Type::Boolean boolean_type;
	Type::Int i8, i16, i32, i64, i128;
	Type::Int u8, u16, u32, u64, u128;
	Type::Real float_type, double_type;

	std::unordered_map<
		std::pair<unsigned int, bool>,
		Type::Int *,
		PairHash<unsigned int, bool>
	> int_type_map;

	std::unordered_map<
		std::pair<Type *, uint32_t>,
		Type::Array *,
		PairHash<Type *, uint32_t>
	> array_type_map;

	std::unordered_map<
		std::pair<Type *, bool>,
		Type::Ref *,
		PairHash<Type *, bool>
	> ref_type_map;

	std::unordered_map<
		std::pair<Type *, bool>,
		Type::Pointer *,
		PairHash<Type *, bool>
	> pointer_type_map;

	struct FieldInfoHash {
		size_t operator()(const Type::Struct::FieldInfo &field) const {
			return hash_combine(std::hash<std::string>()(field.name), std::hash<Type *>()(field.type));
		}
	};

	std::unordered_map<
		std::vector<Type::Struct::FieldInfo>,
		Type::Struct *,
		ArrayHash<Type::Struct::FieldInfo, FieldInfoHash>
	> struct_type_map;

	std::unordered_map<
		std::pair<std::pair<Type *, Type *>, std::vector<Type *>>,
		Type::Function *,
		PairHash<
			std::pair<Type *, Type *>, std::vector<Type *>,
			PairHash<Type *, Type *>, ArrayHash<Type *>
		>
	> function_type_map;
};

} // namespace rin
