
#pragma once

#include "ast.h"
#include "function.h"

namespace rin {

#define INVOKE_ARGS \
    Codegen &g, \
    std::optional<Value> receiver, \
    const std::vector<Value> &args

class Function::Inline : public Function {
public:
	Inline(Type::Function *type, std::vector<std::string> parameter_names, ASTNode *content_node):
		type(type), parameter_names(std::move(parameter_names)), content_node(content_node) {}

	[[nodiscard]] Function *instantiate(INVOKE_ARGS) override;
	[[nodiscard]] Value invoke(INVOKE_ARGS) const override;

	[[nodiscard]] Type::Function *get_type() const { return type; }

	[[nodiscard]] std::string get_type_description() const override {
		return type->to_string();
	}
private:
	Type::Function *type;
	std::vector<std::string> parameter_names;
	ASTNode *content_node;
};

#undef INVOKE_ARGS

} // namespace rin
