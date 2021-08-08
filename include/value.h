
#pragma once

#include <exception>
#include <optional>
#include <utility>

#include <llvm/IR/Value.h>
#include <llvm/IR/Constants.h>

#include "context.h"
#include "type.h"

namespace rin {

class Value {
public:
	static inline Value undef(Type *type) {
		return {
			type,
			llvm::UndefValue::get(type->get_llvm())
		};
	}

	Value(): type(nullptr), llvm_value(nullptr) {} // NOLINT(cppcoreguidelines-pro-type-member-init)

	Value(Type *type, llvm::Value *llvm): // NOLINT(cppcoreguidelines-pro-type-member-init)
		type(type), llvm_value(llvm) {}

	// Type is also a value
	explicit Value(Type *type_value): // NOLINT(cppcoreguidelines-pro-type-member-init)
		type(Type::Self::get_instance()), type_value(type_value) {}

	[[nodiscard]] Type *get_type() const { return type; }
	[[nodiscard]] llvm::Value *get_llvm_value() const { return llvm_value; }
	[[nodiscard]] Type *get_type_value() const { return type_value; }
private:
	Type *type;
	union {
		llvm::Value *llvm_value;
		Type *type_value;
	};
};

} // namespace rin
