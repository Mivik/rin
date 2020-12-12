
#pragma once

#include <unordered_map>

#include <llvm/IR/LLVMContext.h>

#include "type.h"
#include "utility.h"

namespace rin {

class CoreContext {
public:
	CoreContext();
	inline llvm::LLVMContext& get_llvm() { return llvm; }
	inline Type::Void* get_void_type() { return &void_type; }
	inline Type::Boolean* get_boolean_type() { return &boolean_type; }
	inline Type::Int* get_i8_type() { return &i8; }
	inline Type::Int* get_i16_type() { return &i16; }
	inline Type::Int* get_i32_type() { return &i32; }
	inline Type::Int* get_i64_type() { return &i64; }
	inline Type::Int* get_i128_type() { return &i128; }
	inline Type::Int* get_u8_type() { return &u8; }
	inline Type::Int* get_u16_type() { return &u16; }
	inline Type::Int* get_u32_type() { return &u32; }
	inline Type::Int* get_u64_type() { return &u64; }
	inline Type::Int* get_u128_type() { return &u128; }
	inline Type::Real* get_float_type() { return &float_type; }
	inline Type::Real* get_double_type() { return &double_type; }
	Type::Int* get_int_type(unsigned int bit_width, bool is_signed = true);
	Type::Array* get_array_type(Type *element_type, uint32_t size);

	DISABLE_COPY(CoreContext)

	~CoreContext();
private:
	llvm::LLVMContext llvm;
	Type::Void void_type;
	Type::Boolean boolean_type;
	Type::Int i8, i16, i32, i64, i128;
	Type::Int u8, u16, u32, u64, u128;
	Type::Real float_type, double_type;

	struct IntTypeKey {
		const unsigned int bit_width;
		const bool signed_flag;
		IntTypeKey(unsigned int bit_width, bool signed_flag):
			bit_width(bit_width), signed_flag(signed_flag) {}
		inline bool operator==(const IntTypeKey &other) const {
			return bit_width == other.bit_width
				&& signed_flag == other.signed_flag;
		}
		struct Hash {
			inline size_t operator()(const IntTypeKey &key) const {
				return key.bit_width ^ key.signed_flag;
			}
		};
	};
	std::unordered_map<IntTypeKey, Type::Int*, IntTypeKey::Hash> int_type_map;

	struct ArrayTypeKey {
		Type * const element_type;
		const bool size;
		ArrayTypeKey(Type *element_type, uint32_t size):
			element_type(element_type), size(size) {}
		inline bool operator==(const ArrayTypeKey &other) const {
			return element_type == other.element_type
				&& size == other.size;
		}
		struct Hash {
			inline size_t operator()(const ArrayTypeKey &key) const {
				return ((size_t) key.element_type) ^ key.size;
			}
		};
	};
	std::unordered_map<ArrayTypeKey, Type::Array*, ArrayTypeKey::Hash> array_type_map;
};

} // namespace rin
