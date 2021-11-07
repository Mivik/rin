
#include "codegen.h"
#include "ifunc.h"
#include "tfunc.h"

namespace rin {

#define INVOKE_ARGS \
    Codegen &g, \
    std::optional<Value> receiver, \
    const std::vector<Value> &args

Function *Function::Template::instantiate(INVOKE_ARGS) {
	std::vector<Type *> inferred(concepts.size(), nullptr);
	if ((receiver_type_node != nullptr) != receiver.has_value() || args.size() != parameter_type_nodes.size())
		return nullptr;
	for (size_t i = 0; i < args.size(); ++i)
		if (auto name_node = dynamic_cast<ValueNode *>(parameter_type_nodes[i])) {
			auto &param_name = name_node->get_name();
			auto iter = cached_indices.find(param_name);
			if (iter != cached_indices.end()) {
				auto index = iter->second;
				if (!inferred[index]) {
					if (!concepts[index]->satisfy(args[i].get_type())) return nullptr;
					inferred[index] = args[i].get_type();
				} else if (inferred[index] != args[i].get_type()) return nullptr;
				continue;
			}
		}
	g.add_layer(nullptr);
	for (auto &[name, index] : cached_indices) g.declare_value(name, Value(inferred[index]));

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
	auto actual_type = g.get_context().get_function_type(
		receiver_type,
		result_type,
		actual_types
	);
	auto new_name = name;
	new_name += '<';
	for (size_t i = 0; i < inferred.size(); ++i) {
		new_name += inferred[i]->to_string();
		if (i != inferred.size() - 1) new_name += ',';
	}
	new_name += '>';
	if (inline_flag) {
		g.pop_layer(); // TODO !!!!!!!!!!!! ADD PARAMETERS TO INLINE
		return g.declare_function(
			new_name,
			std::make_unique<Inline>(actual_type, parameter_names, content_node.get())
		);
	}
	if (auto func = g.find_function(new_name, actual_type)) { // use functions that already exists
		g.pop_layer();
		return func;
	}
	auto function_object = g.declare_function(actual_type, new_name, false);
	g.implement_function(function_object, parameter_names, content_node.get());
	g.pop_layer();
	return function_object;
}

} // namespace rin
