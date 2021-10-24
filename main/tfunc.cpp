
#include "codegen.h"
#include "tfunc.h"

namespace rin {

#define INVOKE_ARGS \
    Codegen &g, \
    std::optional<Value> receiver, \
    const std::vector<Value> &args

Function *Function::Template::instantiate(INVOKE_ARGS) {
	std::map<Concept *, Type *> inferred;
	if ((receiver_type_node != nullptr) != receiver.has_value() || args.size() != parameter_type_nodes.size())
		return nullptr;
	for (size_t i = 0; i < args.size(); ++i) {
		auto para = parameter_type_nodes[i]->codegen(g);
		if (!para.is_concept_value()) continue;
		auto iter = concepts.find(para.get_concept_value());
		if (iter == concepts.end()) break;
		auto inferred_iter = inferred.find(iter->first);
		if (inferred_iter == inferred.end()) inferred[iter->first] = args[i].get_type();
		else if (inferred_iter->second != args[i].get_type()) return nullptr;
	}
	if (inferred.size() != concepts.size()) return nullptr;
	g.add_layer(nullptr);
	for (auto &[value, type] : inferred) g.declare_value(concepts[value], Value(type));

	auto receiver_type =
		receiver_type_node
		? receiver_type_node->codegen(g).get_type()
		: nullptr;
	auto result_type = result_type_node->codegen(g).get_type();
	if (receiver_type && receiver->get_type() != receiver_type) {
		g.pop_layer();
		return nullptr;
	}
	std::vector<Type *> actual_types(args.size());
	for (size_t i = 0; i < args.size(); ++i) {
		actual_types[i] = parameter_type_nodes[i]->codegen(g).get_type();
		if (args[i].get_type() != actual_types[i]) return nullptr;
	}
	g.pop_layer();
	auto actual_type = g.get_context().get_function_type(
		receiver_type,
		result_type,
		actual_types
	);
	auto function_object = g.declare_function(actual_type, name);
	if (!function_object) return nullptr; // use functions that already exists first
	g.implement_function(function_object, parameter_names, body_node.get());
	return function_object;
}

} // namespace rin
