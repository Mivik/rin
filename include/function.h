
#pragma once

#include <optional>

#include "context.h"
#include "type.h"
#include "utility.h"
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

	DISABLE_COPY(Function)

	virtual ~Function() = default;
protected:
	Function(Type::Function *type): type(type) {}
private:
	Type::Function *type;
};

class Function::Static final : public Function {
public:
	virtual Value invoke(
		Context &ctx,
		std::optional<Value> receiver,
		const std::vector<Value> &args
	) const override;
private:
	Static(Value func):
		Function(dynamic_cast<Type::Function*>(func.get_type())),
		llvm(func.get_llvm()) {}

	llvm::Value *llvm;

	friend class CoreContext;
};

} // namespace rin
