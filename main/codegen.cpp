
#include <memory>

#include "codegen.h"

namespace rin {

Codegen::Codegen(Context &ctx, const std::string &name):
	ctx(ctx),
	module(std::make_unique<llvm::Module>(name, ctx.get_llvm())) {}

llvm::Function *Codegen::get_llvm_function() const {
	if (auto block = get_builder()->GetInsertBlock())
		return block->getParent();
	else throw CodegenException("Top-level context doesn't have parent function");
}

void Codegen::add_layer(
	std::unique_ptr<llvm::IRBuilder<>> builder,
	Function::Static *function
) {
	layers.push_back({
		std::move(builder),
		function
	});
	value_map.add_layer();
	function_map.add_layer();
}

void Codegen::pop_layer() {
	layers.pop_back();
	value_map.pop_layer();
	function_map.pop_layer();
}

} // namespace rin
