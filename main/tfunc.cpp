
#include "codegen.h"
#include "tfunc.h"

namespace rin {

#define INVOKE_ARGS \
    Codegen &g, \
    std::optional<Value> receiver, \
    const std::vector<Value> &args

Function *Function::Template::instantiate(INVOKE_ARGS) {
	do {
		if ((receiver_type != nullptr) != receiver.has_value()) break;
		if (args.size() != parameter_types.size()) break;
		if (receiver_type && !receiver->can_cast_to(receiver_type)) break;
		bool arguments_match = true;
		std::vector<Type *> actual_types(args.size());
		for (size_t i = 0; i < args.size(); ++i) {
			const auto &para = parameter_types[i];
			if (para.is_concept_value()) {
				if (!para.get_concept_value()->satisfy(args[i].get_type())) {
					arguments_match = false;
					break;
				}
				actual_types[i] = args[i].get_type();
			} else if (para.is_type_value()) {
				if (!args[i].can_cast_to(para.get_type_value())) {
					arguments_match = false;
					break;
				}
				actual_types[i] = para.get_type_value();
			}
		}
		if (!arguments_match) break;
		auto actual_type = g.get_context().get_function_type(
			receiver_type,
			result_type,
			actual_types
		);
		auto function_object = g.declare_function(actual_type, name);
		if (!function_object) return nullptr; // use functions that already exists first
		g.implement_function(function_object, parameter_names, body_node.get());
		return function_object;
	} while (false);
	return nullptr;
}

} // namespace rin
