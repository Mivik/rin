
#pragma once

#include <exception>
#include <optional>
#include <utility>

#include <llvm/IR/Value.h>

#include "context.h"
#include "type.h"

namespace rin {

class CastException : public std::exception {
public:
	[[nodiscard]] const char *what() const noexcept override { return msg.data(); }

	explicit CastException(std::string msg): msg(std::move(msg)) {}
	CastException(Type *from, Type *to):
			CastException("Cannot cast " + from->to_string() + " into " + to->to_string()) {}
private:
	std::string msg;
};

class Value {
public:
	static inline bool can_cast(Context &ctx, Type *from, Type *to, bool implicit_only = true) {
		return Value(from, llvm::UndefValue::get(from->get_llvm())).can_cast(ctx, to, implicit_only);
	}
	static inline Value undef(Type *type) {
		return {
				type,
				llvm::UndefValue::get(type->get_llvm())
		};
	}

	Value(): type(nullptr), llvm(nullptr) {}

	Value(Type *type, llvm::Value *llvm):
			type(type), llvm(llvm) {}

	Value cast(Context &ctx, Type *to, bool implicit_only = true, bool check_only = false) const;

	inline bool can_cast(Context &ctx, Type *to, bool implicit_only = true) const {
		try {
			cast(ctx, to, implicit_only, true);
			return true;
		} catch (const CastException &e) { return false; }
	}
	inline std::optional<Value> safe_cast(Context &ctx, Type *to) const {
		try {
			return cast(ctx, to, true);
		} catch (const CastException &e) { return std::nullopt; }
	}
	[[nodiscard]] inline Type *get_type() const { return type; }
	[[nodiscard]] inline llvm::Value *get_llvm() const { return llvm; }
	[[nodiscard]] inline bool is_ref() const { return dynamic_cast<Type::Ref *>(type); }
	Value deref(Context &ctx) const;
	Value pointer_subscript(Context &ctx) const;
	Value pointer_subscript(Context &ctx, Value index) const;
private:
	Type *type;
	llvm::Value *llvm;
};

} // namespace rin
