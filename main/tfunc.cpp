
#include "codegen.h"
#include "tfunc.h"

namespace rin {

#define INVOKE_ARGS \
    Codegen &g, \
    std::optional<Value> receiver, \
    const std::vector<Value> &args

Function *Function::Template::instantiate(INVOKE_ARGS) {
	std::map<std::string, Type *> inferred;
	if ((receiver_type_node != nullptr) != receiver.has_value() || args.size() != parameter_type_nodes.size())
		return nullptr;
	for (size_t i = 0; i < args.size(); ++i)
		if (auto name_node = dynamic_cast<ValueNode *>(parameter_type_nodes[i])) {
			auto name = name_node->get_name();
			auto iter = concepts.find(name);
			if (iter != concepts.end()) {
				auto inferred_iter = inferred.find(name);
				if (inferred_iter == inferred.end()) inferred[name] = args[i].get_type();
				else if (inferred_iter->second != args[i].get_type()) return nullptr;
				continue;
			}
		}
	if (inferred.size() != concepts.size()) return nullptr;
	g.add_layer(nullptr);
	for (auto &[name, type] : inferred) g.declare_value(name, Value(type));

	auto receiver_type =
		receiver_type_node
		? receiver_type_node->codegen(g).get_type_value()
		: nullptr;
	auto result_type = result_type_node->codegen(g).get_type_value();
	if (receiver_type && receiver->get_type() != receiver_type) {
		g.pop_layer();
		return nullptr;
	}
	std::vector<Type *> actual_types(args.size());
	for (size_t i = 0; i < args.size(); ++i) {
		actual_types[i] = parameter_type_nodes[i]->codegen(g).get_type_value();
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
