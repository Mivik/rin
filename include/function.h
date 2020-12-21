
#pragma once

#include <optional>

#include "context.h"
#include "type.h"
#include "value.h"

namespace rin {

class Function {
public:
	class Static;

	virtual Value invoke(
		Context &ctx,
		std::optional<Value> receiver,
		const std::vector<Value> &args
	) const = 0;

	inline Type::Function* get_type() const { return type; }
protected:
	Function(Type::Function *type): type(type) {}
private:
	Type::Function *type;
};

class Function::Static final : public Function {
public:
	Static(Value func):
		Function(dynamic_cast<Type::Function*>(func.get_type())),
		llvm(func.get_llvm()) {}

	virtual Value invoke(
		Context &ctx,
		std::optional<Value> receiver,
		const std::vector<Value> &args
	) const override;
private:
	llvm::Value *llvm;
};

} // namespace rin
