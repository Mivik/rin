
#pragma once

#include <cstdint>

#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>

#include "utility.h"

namespace rin {

class CoreContext;

class Type {
public:
	class Array;
	class Int;
	class Real;

	inline llvm::Type* get_llvm() const { return llvm; }
	virtual bool operator==(const Type &other) const { return llvm == other.llvm; }
protected:
	Type(llvm::Type *llvm): llvm(llvm) {}
private:
	llvm::Type* const llvm;
};

class Type::Int : public Type {
public:
	inline unsigned int get_bit_width() const {
		return ptr_cast<llvm::IntegerType>(llvm)->getIntegerBitWidth();
	}
	inline bool is_signed() const { return signed_flag; }
	bool operator==(const Type &other) const override;
private:
	Int(CoreContext *ctx, unsigned int bit_width, bool is_signed = true);
	const bool signed_flag;

	friend class CoreContext;
};

class Type::Real : public Type {
private:
	Real(llvm::Type *llvm): Type(llvm) {}

	friend class CoreContext;
};

class Type::Array : public Type {
public:
	inline Type* get_element_type() const { return element_type; }
	inline uint32_t get_size() const { return size; }
private:
	Array(CoreContext *ctx, Type *element_type, uint32_t size);
	Type* const element_type;
	uint32_t size;

	friend class CoreContext;
};

} // namespace rin
