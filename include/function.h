
#pragma once

#include <functional>
#include <optional>

#include "context.h"
#include "type.h"
#include "utility.h"
#include "value.h"

namespace rin {

class Codegen;

class Function {
public:
	class Builtin;

	class Static;

	virtual ~Function() = default;

	virtual Value invoke(
		Codegen &ctx,
		std::optional<Value> receiver,
		const std::vector<Value> &args
	) const = 0;

	[[nodiscard]] Type::Function *get_type() const { return type; }

	DISABLE_COPY(Function)
protected:
	explicit Function(Type::Function *type): type(type) {}
private:
	Type::Function *type;
};

class Function::Builtin final : public Function {
public:
	using FuncType = std::function<Value(
		Codegen &,
		std::optional<Value>,
		const std::vector<Value> &
	)>;

	Value invoke(
		Codegen &g,
		std::optional<Value> receiver,
		const std::vector<Value> &args
	) const override {
		return function(g, receiver, args);
	}
private:
	explicit Builtin(Type::Function *type, FuncType func):
		Function(type),
		function(std::move(func)) {}

	FuncType function;
};

class Function::Static final : public Function {
public:
	Value invoke(
		Codegen &g,
		std::optional<Value> receiver,
		const std::vector<Value> &args
	) const override;
private:
	explicit Static(Value func):
		Function(dynamic_cast<Type::Function *>(func.get_type())),
		llvm(func.get_llvm_value()) {}

	llvm::Value *llvm;

	friend class Context;
};

} // namespace rin
