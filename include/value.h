
#pragma once

#include <llvm/IR/Value.h>

#include <optional>

#include "context.h"
#include "type.h"

namespace rin {

class CastException : public std::exception {
public:
	const char *what() const noexcept { return msg.data(); }

	CastException(const std::string &msg): msg(msg) {}
	CastException(Type *from, Type *to):
		CastException("Cannot cast " + from->to_string() + " into " + to->to_string()) {}
private:
	std::string msg;
};

class Value {
public:
	static inline bool can_cast(Context &ctx, Type *from, Type *to, bool implicit_only = false) {
		return Value(from, llvm::UndefValue::get(from->get_llvm())).can_cast(ctx, to, implicit_only);
	}
	static inline Value undef(Type *type) {
		return {
			type,
			llvm::UndefValue::get(type->get_llvm())
		};
	}

	Value(Type *type, llvm::Value *llvm):
		type(type), llvm(llvm) {}

	Value cast(Context &ctx, Type *to, bool implicit_only = false, bool check_only = false) const;

	inline bool can_cast(Context &ctx, Type *to, bool implicit_only = false) const {
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
	inline Type* get_type() const { return type; }
	inline llvm::Value* get_llvm() const { return llvm; }
	inline bool is_ref() const { return dynamic_cast<Type::Ref*>(type); }
	Value deref(Context &ctx);
	Value pointer_subscript(Context &ctx);
	Value pointer_subscript(Context &ctx, Value index);
private:
	Type *type;
	llvm::Value *llvm;
};

} // namespace rin
