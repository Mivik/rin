
#include "codegen.h"
#include "function.h"

#define INVOKE_ARGS \
    Codegen &g, \
    std::optional<Value> receiver, \
    const std::vector<Value> &args

namespace rin {

Function *Function::instantiate(INVOKE_ARGS) {
	auto receiver_type = type->get_receiver_type();
	auto parameter_types = type->get_parameter_types();
	do {
		if ((receiver_type == nullptr) != receiver.has_value()) break;
		if (args.size() != parameter_types.size()) break;
		if (receiver_type && !receiver->can_cast_to(receiver_type)) break;
		bool arguments_match = true;
		for (size_t i = 0; i < args.size(); ++i)
			if (!args[i].can_cast_to(parameter_types[i])) {
				arguments_match = false;
				break;
			}
		if (!arguments_match) break;
		return this;
	} while (false);
	return nullptr;
}

Value Function::Static::invoke(INVOKE_ARGS) const {
	if (g.is_const_eval()) throw std::runtime_error("Not Implemented");
	std::vector<llvm::Value *> llvm_args;
	const bool has_receiver = receiver.has_value();
	llvm_args.resize(args.size() + has_receiver);
	for (size_t i = 0; i < args.size(); ++i)
		llvm_args[i + has_receiver] = args[i].get_llvm_value();
	if (has_receiver)
		llvm_args[0] = receiver->get_llvm_value();
	return {
		get_type()->get_result_type(),
		g.get_builder()->CreateCall(
			llvm::dyn_cast<llvm::FunctionType>(get_type()->get_llvm()),
			llvm,
			llvm_args
		)
	};
}

} // namespace rin
