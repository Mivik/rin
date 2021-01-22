
#pragma once

#include <cstdint>
#include <utility>
#include <vector>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

#include "utility.h"

namespace rin {

class CoreContext;

class Type {
public:
	class Array;

	class Boolean;

	class Function;

	class Int;

	class Nothing;

	class Pointer;

	class Real;

	class Ref;

	class Void;

	Type *deref(CoreContext &ctx);
	[[nodiscard]] inline llvm::Type *get_llvm() const { return llvm; }
	[[nodiscard]] inline size_t scalar_size_in_bits() const { return llvm->getScalarSizeInBits(); }
	virtual bool operator==(const Type &other) const { return llvm == other.llvm; }
	[[nodiscard]] virtual bool is_primitive() const { return false; }
	[[nodiscard]] virtual std::string to_string() const = 0;

	DISABLE_COPY(Type)
protected:
	explicit Type(llvm::Type *llvm): llvm(llvm) {}
private:
	llvm::Type *llvm;
};

class Type::Void final : public Type {
public:
	[[nodiscard]] std::string to_string() const override { return "void"; }
private:
	explicit Void(CoreContext *ctx);

	friend class CoreContext;
};

class Type::Boolean final : public Type {
public:
	[[nodiscard]] std::string to_string() const override { return "bool"; }
private:
	explicit Boolean(CoreContext *ctx);

	friend class CoreContext;
};

class Type::Int final : public Type {
public:
	[[nodiscard]] inline unsigned int get_bit_width() const {
		return direct_cast<llvm::IntegerType>(llvm)->getIntegerBitWidth();
	}
	[[nodiscard]] inline bool is_signed() const { return signed_flag; }
	bool operator==(const Type &other) const override;
	[[nodiscard]] bool is_primitive() const override { return true; }
	[[nodiscard]] std::string to_string() const override;
private:
	Int(CoreContext *ctx, unsigned int bit_width, bool is_signed = true);
	bool signed_flag;

	friend class CoreContext;
};

class Type::Real final : public Type {
public:
	[[nodiscard]] bool is_primitive() const override { return true; }
	[[nodiscard]] std::string to_string() const override { return name; }
private:
	explicit Real(llvm::Type *llvm, std::string name = "[unknown real type]"):
		Type(llvm), name(std::move(name)) {}

	std::string name;

	friend class CoreContext;
};

class Type::Array final : public Type {
public:
	[[nodiscard]] inline Type *get_element_type() const { return element_type; }
	[[nodiscard]] inline uint32_t get_size() const { return size; }
	[[nodiscard]] std::string to_string() const override;
private:
	Array(Type *element_type, uint32_t size);
	Type *element_type;
	uint32_t size;

	friend class CoreContext;
};

class Type::Pointer final : public Type {
public:
	[[nodiscard]] std::string to_string() const override;
	[[nodiscard]] inline Type *get_sub_type() const { return sub_type; }
	[[nodiscard]] inline bool is_const() const { return const_flag; }
private:
	Pointer(Type *sub_type, bool const_flag);

	Type *sub_type;
	bool const_flag;

	friend class CoreContext;
};

class Type::Ref final : public Type {
public:
	[[nodiscard]] std::string to_string() const override;
	[[nodiscard]] inline Type *get_sub_type() const { return sub_type; }
	[[nodiscard]] inline bool is_const() const { return const_flag; }
private:
	Ref(Type *sub_type, bool const_flag);

	Type *sub_type;
	bool const_flag;

	friend class CoreContext;
};

class Type::Function final : public Type {
public:
	[[nodiscard]] std::string to_string() const override;
	[[nodiscard]] inline Type *get_receiver_type() const { return receiver_type; }
	[[nodiscard]] inline Type *get_result_type() const { return result_type; }
	[[nodiscard]] inline std::vector<Type *> get_parameter_types() const { return param_types; }
private:
	Function(Type *receiver_type, Type *result_type, const std::vector<Type *> &param_types);

	Type *receiver_type, *result_type;
	std::vector<Type *> param_types;

	friend class CoreContext;
};

class Type::Nothing final : public Type {
public:
	[[nodiscard]] std::string to_string() const override;
private:
	explicit Nothing(CoreContext *ctx);

	friend class CoreContext;
};

} // namespace rin
