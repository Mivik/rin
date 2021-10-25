
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
		std::map<std::string, Concept *> concepts,
		ASTNode *receiver_type_node,
		ASTNode *result_type_node,
		std::vector<ASTNode *> parameter_type_nodes, // can be either concept or type
		std::vector<std::string> parameter_names,
		Ptr<BlockNode> body_node
	): name(std::move(name)),
	   concepts(std::move(concepts)),
	   receiver_type_node(receiver_type_node),
	   result_type_node(result_type_node),
	   parameter_type_nodes(std::move(parameter_type_nodes)),
	   parameter_names(std::move(parameter_names)),
	   body_node(std::move(body_node)) {}

	[[nodiscard]] Value invoke(INVOKE_ARGS) const override {
		g.error("Trying to invoke an abstract template function");
	}

	[[nodiscard]] bool is_const_eval() const override { return false; } // TODO

	[[nodiscard]] Function *instantiate(INVOKE_ARGS) override;
private:
	std::string name;
	std::map<std::string, Concept *> concepts;
	ASTNode *receiver_type_node, *result_type_node;
	std::vector<ASTNode *> parameter_type_nodes;
	std::vector<std::string> parameter_names;
	Ptr<BlockNode> body_node;
};

#undef INVOKE_ARGS

} // namespace rin
