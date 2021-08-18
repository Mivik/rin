
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

	[[nodiscard]] virtual bool is_const_eval() const = 0;

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

	Builtin(Type::Function *type, FuncType func):
		Function(type),
		function(std::move(func)) {}

	Value invoke(
		Codegen &g,
		std::optional<Value> receiver,
		const std::vector<Value> &args
	) const override {
		return function(g, receiver, args);
	}

	// TODO is it?
	[[nodiscard]] bool is_const_eval() const override { return true; }

private:
	FuncType function;
};

class Function::Static final : public Function {
public:
	explicit Static(Value func, bool const_evaluated):
		Function(dynamic_cast<Type::Function *>(func.get_type())),
		llvm(func.get_llvm_value()),
		const_eval(const_evaluated) {}

	Value invoke(
		Codegen &g,
		std::optional<Value> receiver,
		const std::vector<Value> &args
	) const override;

	[[nodiscard]] bool is_const_eval() const override { return const_eval; }
private:
	llvm::Value *llvm;
	bool const_eval;
};

} // namespace rin
