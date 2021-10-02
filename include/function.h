
#pragma once

#include <functional>
#include <optional>

#include "context.h"
#include "type.h"
#include "utility.h"
#include "value.h"

namespace rin {

#define INVOKE_ARGS \
    Codegen &g, \
    std::optional<Value> receiver, \
    const std::vector<Value> &args

class Codegen;

class Function {
public:
	class Builtin;

	class Static;

	class Template;

	virtual ~Function() = default;

	virtual Function *instantiate(INVOKE_ARGS);

	virtual Value invoke(INVOKE_ARGS) const = 0;

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
	using FuncType = std::function<Value(INVOKE_ARGS)>;

	Builtin(Type::Function *type, FuncType func):
		Function(type),
		function(std::move(func)) {}

	Value invoke(INVOKE_ARGS) const override {
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
		llvm(llvm::dyn_cast<llvm::Function>(func.get_llvm_value())),
		const_eval(const_evaluated) {}

	Value invoke(INVOKE_ARGS) const override;

	[[nodiscard]] llvm::Function *get_llvm_value() const { return llvm; }
	[[nodiscard]] bool is_const_eval() const override { return const_eval; }
private:
	llvm::Function *llvm;
	bool const_eval;
};

#undef INVOKE_ARGS

} // namespace rin
