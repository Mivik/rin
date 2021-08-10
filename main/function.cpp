
#include "codegen.h"
#include "function.h"

namespace rin {

Value Function::Static::invoke(
	Codegen &g,
	std::optional<Value> receiver,
	const std::vector<Value> &args
) const {
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
