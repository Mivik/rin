
#include "ast.h"
#include "function.h"

namespace rin {

#define INVOKE_ARGS \
    Codegen &g, \
    std::optional<Value> receiver, \
    const std::vector<Value> &args

class Function::Template final : public Function {
public:
	explicit Template(
		std::string name,
		Type *receiver_type,
		Type *result_type,
		std::vector<Value> parameter_types, // can be either concept or type
		std::vector<std::string> parameter_names,
		Ptr<BlockNode> body_node
	): Function(nullptr),
	   name(std::move(name)),
	   receiver_type(receiver_type),
	   result_type(result_type),
	   parameter_types(std::move(parameter_types)),
	   parameter_names(std::move(parameter_names)),
	   body_node(std::move(body_node)) {}

	[[nodiscard]] Value invoke(INVOKE_ARGS) const override {
		g.error("Trying to invoke an abstract template function");
	}

	[[nodiscard]] bool is_const_eval() const override { return false; } // TODO

	[[nodiscard]] Function *instantiate(INVOKE_ARGS) override;
private:
	std::string name;
	Type *receiver_type, *result_type;
	std::vector<Value> parameter_types;
	std::vector<std::string> parameter_names;
	Ptr<BlockNode> body_node;
};

#undef INVOKE_ARGS

} // namespace rin
