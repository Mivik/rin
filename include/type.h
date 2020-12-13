
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
	class Boolean;
	class Int;
	class Real;
	class Void;

	inline llvm::Type* get_llvm() const { return llvm; }
	inline size_t scalar_size_in_bits() const { return llvm->getScalarSizeInBits(); }
	virtual bool operator==(const Type &other) const { return llvm == other.llvm; }
	virtual bool is_primitive() const { return false; }
	virtual std::string to_string() const = 0;
protected:
	Type(llvm::Type *llvm): llvm(llvm) {}
private:
	llvm::Type *llvm;
};

class Type::Void final : public Type {
public:
	std::string to_string() const override { return "void"; }
private:
	Void(CoreContext *ctx);

	friend class CoreContext;
};

class Type::Boolean final : public Type {
public:
	std::string to_string() const override { return "bool"; }
private:
	Boolean(CoreContext *ctx);

	friend class CoreContext;
};

class Type::Int final : public Type {
public:
	inline unsigned int get_bit_width() const {
		return ptr_cast<llvm::IntegerType>(llvm)->getIntegerBitWidth();
	}
	inline bool is_signed() const { return signed_flag; }
	bool operator==(const Type &other) const override;
	bool is_primitive() const override { return true; }
	std::string to_string() const override;
private:
	Int(CoreContext *ctx, unsigned int bit_width, bool is_signed = true);
	bool signed_flag;

	friend class CoreContext;
};

class Type::Real final : public Type {
public:
	bool is_primitive() const override { return true; }
	std::string to_string() const override { return name; }
private:
	Real(llvm::Type *llvm, const std::string &name = "[unknown real type]"):
		Type(llvm), name(name) {}

	std::string name;

	friend class CoreContext;
};

class Type::Array final : public Type {
public:
	inline Type* get_element_type() const { return element_type; }
	inline uint32_t get_size() const { return size; }
	std::string to_string() const override;
private:
	Array(CoreContext *ctx, Type *element_type, uint32_t size);
	Type * element_type;
	uint32_t size;

	friend class CoreContext;
};

} // namespace rin
