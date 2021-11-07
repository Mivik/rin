
#include "codegen.h"
#include "function.h"
#include "ifunc.h"

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
