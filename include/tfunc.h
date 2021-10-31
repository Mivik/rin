
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
		std::vector<std::pair<std::string, Concept *>> concepts,
		ASTNode *receiver_type_node,
		ASTNode *result_type_node,
		std::vector<ASTNode *> parameter_type_nodes, // can be either concept or type
		std::vector<std::string> parameter_names,
		Ptr<ASTNode> content_node
	): name(std::move(name)),
	   receiver_type_node(receiver_type_node),
	   result_type_node(result_type_node),
	   parameter_type_nodes(std::move(parameter_type_nodes)),
	   parameter_names(std::move(parameter_names)),
	   content_node(std::move(content_node)) {
		this->concepts.reserve(concepts.size());
		for (auto &[name, concept_value] : concepts) {
			cached_indices[name] = this->concepts.size();
			this->concepts.push_back(concept_value);
		}
	}

	[[nodiscard]] Value invoke(INVOKE_ARGS) const override {
		g.error("Trying to invoke an abstract template function");
	}

	[[nodiscard]] bool is_const_eval() const override { return false; } // TODO
	// TODO type description of tfunc should not be used, but we should implement it
	[[nodiscard]] std::string get_type_description() const override { return ""; }

	[[nodiscard]] Function *instantiate(INVOKE_ARGS) override;
private:
	std::string name;
	std::vector<Concept *> concepts;
	std::map<std::string, size_t> cached_indices;
	ASTNode *receiver_type_node, *result_type_node;
	std::vector<ASTNode *> parameter_type_nodes;
	std::vector<std::string> parameter_names;
	Ptr<ASTNode> content_node;
};

#undef INVOKE_ARGS

} // namespace rin
