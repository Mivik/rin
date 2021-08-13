
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
		const std::vector<Value> &args,
		bool const_eval = false
	) const = 0;

	[[nodiscard]] virtual bool is_const_evaluated() const = 0;

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
		const std::vector<Value> &args,
		bool
	) const override {
		return function(g, receiver, args);
	}

	// TODO is it?
	[[nodiscard]] bool is_const_evaluated() const override { return true; }
private:
	explicit Builtin(Type::Function *type, FuncType func):
		Function(type),
		function(std::move(func)) {}

	FuncType function;

	friend class Codegen;
};

class Function::Static final : public Function {
public:
	Value invoke(
		Codegen &g,
		std::optional<Value> receiver,
		const std::vector<Value> &args,
		bool const_eval
	) const override;

	[[nodiscard]] bool is_const_evaluated() const override { return const_evaluated; }
private:
	explicit Static(Value func, bool const_evaluated):
		Function(dynamic_cast<Type::Function *>(func.get_type())),
		llvm(func.get_llvm_value()),
		const_evaluated(const_evaluated) {}

	llvm::Value *llvm;
	bool const_evaluated;

	friend class Context;
};

} // namespace rin
