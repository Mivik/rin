
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

	class Inline;

	class Static;

	class Template;

	virtual ~Function() = default;

	virtual Function *instantiate(INVOKE_ARGS) = 0;

	virtual Value invoke(INVOKE_ARGS) const = 0;

	[[nodiscard]] virtual std::string get_type_description() const = 0;

	DISABLE_COPY(Function)
protected:
	explicit Function() = default;
};

class Function::Builtin final : public Function {
public:
	using VerifierType = std::function<bool(INVOKE_ARGS)>;
	using FuncType = std::function<Value(INVOKE_ARGS)>;

	Builtin(std::string type_desc, VerifierType verifier, FuncType func):
		type_description(std::move(type_desc)),
		verifier(std::move(verifier)),
		function(std::move(func)) {}

	Function *instantiate(Codegen &g, std::optional<Value> receiver, const std::vector<Value> &args) override {
		if (verifier(g, receiver, args)) return this;
		return nullptr;
	}

	Value invoke(INVOKE_ARGS) const override {
		return function(g, receiver, args);
	}

	[[nodiscard]] std::string get_type_description() const override { return type_description; }

private:
	std::string type_description;
	VerifierType verifier;
	FuncType function;
};

class Function::Static final : public Function {
public:
	explicit Static(Value func):
		type(dynamic_cast<Type::Function *>(func.get_type())),
		llvm(llvm::dyn_cast<llvm::Function>(func.get_llvm_value())) {}

	Function *instantiate(INVOKE_ARGS) override;
	Value invoke(INVOKE_ARGS) const override;

	[[nodiscard]] Type::Function *get_type() const { return type; }
	[[nodiscard]] llvm::Function *get_llvm_value() const { return llvm; }

	[[nodiscard]] std::string get_type_description() const override {
		return type->to_string();
	}

private:
	Type::Function *type;
	llvm::Function *llvm;
};

#undef INVOKE_ARGS

} // namespace rin
