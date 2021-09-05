
#pragma once

#include <cstdint>
#include <utility>
#include <vector>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

#include "utility.h"

namespace rin {

class Context;

class Value;

class Type {
public:
	class Array;

	class Boolean;

	class Function;

	class Int;

	class Pointer;

	class Real;

	class Ref;

	class Self;

	class Struct;

	class Tuple;

	class Void;

	static Type *common_type(Type *A, Type *B);

	[[nodiscard]] llvm::Type *get_llvm() const { return llvm; }
	[[nodiscard]] bool is_abstract() const { return llvm == nullptr; }
	[[nodiscard]] virtual Type *get_element(unsigned) const { // pass -1 if the index is dynamic
		throw std::runtime_error("No element contained in this type");
	}
	[[nodiscard]] virtual std::string to_string() const = 0;

	[[nodiscard]] Type *deref();

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
	explicit Void(Context *ctx);

	friend class Context;
};

class Type::Boolean final : public Type {
public:
	[[nodiscard]] std::string to_string() const override { return "bool"; }
private:
	explicit Boolean(Context *ctx);

	friend class Context;
};

class Type::Int final : public Type {
public:
	[[nodiscard]] unsigned get_bit_width() const {
		return direct_cast<llvm::IntegerType>(llvm)->getIntegerBitWidth();
	}
	[[nodiscard]] bool is_signed() const { return signed_flag; }

	[[nodiscard]] std::string to_string() const override {
		return "ui"[signed_flag] + std::to_string(get_bit_width());
	}
private:
	Int(Context *ctx, unsigned int bit_width, bool is_signed = true);
	bool signed_flag;

	friend class Context;
};

class Type::Real final : public Type {
public:
	[[nodiscard]] std::string to_string() const override { return name; }
private:
	explicit Real(llvm::Type *llvm, std::string name = "[unknown real type]"):
		Type(llvm), name(std::move(name)) {}

	std::string name;

	friend class Context;
};

class Type::Array final : public Type {
public:
	[[nodiscard]] Type *get_element_type() const { return element_type; }
	[[nodiscard]] uint32_t get_size() const { return size; }

	[[nodiscard]] Type *get_element(unsigned) const override { return element_type; }
	[[nodiscard]] std::string to_string() const override {
		return '[' + element_type->to_string() + "; " + std::to_string(size) + ']';
	}
private:
	Array(Type *element_type, uint32_t size);

	Type *element_type;
	uint32_t size;

	friend class Context;
};

class Type::Pointer final : public Type {
public:
	[[nodiscard]] Type *get_element(unsigned) const override { return sub_type; }
	[[nodiscard]] Type *get_sub_type() const { return sub_type; }
	[[nodiscard]] bool is_const() const { return const_flag; }
	[[nodiscard]] std::string to_string() const override {
		return (const_flag? "*const ": "*") + sub_type->to_string();
	}
private:
	Pointer(Type *sub_type, bool const_flag);

	Type *sub_type;
	bool const_flag;

	friend class Context;
};

class Type::Ref final : public Type {
public:
	[[nodiscard]] Type *get_sub_type() const { return sub_type; }
	[[nodiscard]] bool is_const() const { return const_flag; }
	[[nodiscard]] std::string to_string() const override {
		return (const_flag? "&const ": "&") + sub_type->to_string();
	}
private:
	Ref(Type *sub_type, bool const_flag);

	Type *sub_type;
	bool const_flag;

	friend class Context;
};

class Type::Self final : public Type {
public:
	static Type::Self *get_instance() {
		static Type::Self instance;
		return &instance;
	}
	[[nodiscard]] std::string to_string() const override {
		return "type";
	}
private:
	Self(): Type(nullptr) {}
};

class Type::Struct final : public Type {
public:
	struct FieldInfo {
		std::string name;
		Type *type;

		bool operator==(const FieldInfo &other) const {
			return name == other.name && type == other.type;
		}
	};

	[[nodiscard]] std::optional<size_t> find_index_by_name(const std::string &name) const {
		for (size_t i = 0; i < fields.size(); ++i)
			if (fields[i].name == name) return i;
		return std::nullopt;
	}

	[[nodiscard]] const std::vector<FieldInfo> &get_fields() const { return fields; }

	[[nodiscard]] Type *get_element(unsigned index) const override {
		if (index >= fields.size())
			throw std::runtime_error("Type element index out of bound"); // TODO error message
		return fields[index].type;
	}
	[[nodiscard]] std::string to_string() const override;
private:
	Struct(Context *ctx, std::vector<FieldInfo> fields);

	std::vector<FieldInfo> fields;

	friend class Context;
};

class Type::Tuple final : public Type {
public:
	[[nodiscard]] const std::vector<Type *> &get_element_types() const { return elements; }

	[[nodiscard]] Type *get_element(unsigned index) const override {
		if (index >= elements.size())
			throw std::runtime_error("Type element index out of bound"); // TODO error message
		return elements[index];
	}
	[[nodiscard]] std::string to_string() const override;
private:
	Tuple(Context *ctx, std::vector<Type *> elements);

	std::vector<Type *> elements;

	friend class Context;
};

class Type::Function final : public Type {
public:
	[[nodiscard]] Type *get_receiver_type() const { return receiver_type; }
	[[nodiscard]] Type *get_result_type() const { return result_type; }
	[[nodiscard]] std::vector<Type *> get_parameter_types() const { return param_types; }
	[[nodiscard]] std::string to_string(const std::string &name) const;
	[[nodiscard]] std::string to_string() const override {
		return to_string("");
	}
private:
	Function(Type *receiver_type, Type *result_type, const std::vector<Type *> &param_types);

	Type *receiver_type, *result_type;
	std::vector<Type *> param_types;

	friend class Context;
};

} // namespace rin
