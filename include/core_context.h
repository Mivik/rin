
#pragma once

#include <unordered_map>

#include <llvm/IR/LLVMContext.h>

#include "type.h"
#include "utility.h"

namespace rin {

class Function;

class Value;

class CoreContext {
public:
	CoreContext();
	inline llvm::LLVMContext &get_llvm() { return llvm; }
	inline Type::Void *get_void_type() { return &void_type; }
	inline Type::Nothing *get_nothing_type() { return &nothing_type; }
	inline Type::Boolean *get_boolean_type() { return &boolean_type; }
	inline Type::Int *get_i8_type() { return &i8; }
	inline Type::Int *get_i16_type() { return &i16; }
	inline Type::Int *get_i32_type() { return &i32; }
	inline Type::Int *get_i64_type() { return &i64; }
	inline Type::Int *get_i128_type() { return &i128; }
	inline Type::Int *get_u8_type() { return &u8; }
	inline Type::Int *get_u16_type() { return &u16; }
	inline Type::Int *get_u32_type() { return &u32; }
	inline Type::Int *get_u64_type() { return &u64; }
	inline Type::Int *get_u128_type() { return &u128; }
	inline Type::Real *get_float_type() { return &float_type; }
	inline Type::Real *get_double_type() { return &double_type; }
	Type::Int *get_int_type(unsigned int bit_width, bool is_signed = true);
	Type::Array *get_array_type(Type *element_type, uint32_t size);
	Type::Ref *get_ref_type(Type *type, bool const_flag = false);
	Type::Pointer *get_pointer_type(Type *type, bool const_flag = false);
	Type::Function *get_function_type(
		Type *receiver_type, Type *result_type,
		const std::vector<Type *> &param_types
	);
	Value get_void();
	Value get_nothing();

	Function *create_function(const std::string &name, const Value &value);

	const std::vector<Function *> &lookup_functions(const std::string &name);

	DISABLE_COPY(CoreContext)

	~CoreContext();
private:
	llvm::LLVMContext llvm;
	Type::Void void_type;
	Type::Nothing nothing_type;
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

	struct ArrayHash {
		inline size_t operator()(const std::vector<Type *> &arr) const {
			size_t ret = 0;
			for (auto element : arr) ret = hash_combine(ret, std::hash<Type *>()(element));
			return ret;
		}
	};

	std::unordered_map<
		std::pair<std::pair<Type *, Type *>, std::vector<Type *>>,
		Type::Function *,
		PairHash<std::pair<Type *, Type *>, std::vector<Type *>,
			PairHash<Type *, Type *>, ArrayHash>
	> function_type_map;

	std::unordered_map<
		std::pair<std::string, Type::Function *>,
		Function *,
		PairHash<std::string, Type::Function *>
	> function_map;

	std::unordered_map<std::string, std::vector<Function *>> function_index;
};

} // namespace rin
