
#include "codegen.h"
#include "function.h"
#include "ifunc.h"
#include "tfunc.h"

#define INVOKE_ARGS \
    Codegen &g, \
    std::optional<Value> receiver, \
    const std::vector<Value> &args

namespace rin {

inline bool arguments_match(Type::Function *type, std::optional<Value> receiver, const std::vector<Value> &args) {
	auto receiver_type = type->get_receiver_type();
	auto parameter_types = type->get_parameter_types();
	do {
		if ((receiver_type != nullptr) != receiver.has_value()) break;
		if (args.size() != parameter_types.size()) break;
		if (receiver_type && receiver->get_type() != receiver_type) break;
		bool arguments_match = true;
		for (size_t i = 0; i < args.size(); ++i)
			if (args[i].get_type() != parameter_types[i]) {
				arguments_match = false;
				break;
			}
		if (!arguments_match) break;
		return true;
	} while (false);
	return false;
}

Function *Function::Static::instantiate(INVOKE_ARGS) {
	return arguments_match(type, receiver, args)? this: nullptr;
}

Value Function::Static::invoke(INVOKE_ARGS) const {
	if (g.is_inlined()) throw std::runtime_error("Not supported");
	std::vector<llvm::Value *> llvm_args;
	const bool has_receiver = receiver.has_value();
	llvm_args.resize(args.size() + has_receiver);
	for (size_t i = 0; i < args.size(); ++i)
		llvm_args[i + has_receiver] = args[i].get_llvm_value();
	if (has_receiver)
		llvm_args[0] = receiver->get_llvm_value();
	return g.create_value(
		get_type()->get_result_type(),
		g.get_builder()->CreateCall(
			llvm::dyn_cast<llvm::FunctionType>(get_type()->get_llvm()),
			llvm,
			llvm_args
		)
	);
}

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
					if (!g.find_impl(args[i].get_type(), concepts[index]).has_value()) return nullptr;
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

Function *Function::Inline::instantiate(INVOKE_ARGS) {
	return arguments_match(type, receiver, args)? this: nullptr;
}

Value Function::Inline::invoke(INVOKE_ARGS) const {
	Value result;
	auto sub_entry = llvm::BasicBlock::Create(
		g.get_llvm_context(),
		"sub_entry",
		g.get_builder()->GetInsertBlock()->getParent()
	);
	g.get_builder()->CreateBr(sub_entry);
	auto sub = g.derive_inline_context(type->get_result_type(), result);
	sub->add_layer(
		type,
		std::make_unique<llvm::IRBuilder<>>(sub_entry)
	);
	sub->implement_function(type, parameter_names, receiver, args, content_node);
	g.dispose_inline_context(sub);
	return result;
}

} // namespace rin
