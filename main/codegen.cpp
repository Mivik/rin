
#include <memory>

#include "codegen.h"

namespace rin {

Codegen::Codegen(Context &ctx, const std::string &name):
	ctx(ctx),
	module(std::make_unique<llvm::Module>(name, ctx.get_llvm())) {
	add_layer(std::make_unique<llvm::IRBuilder<>>(ctx.get_llvm()), nullptr);
#define ARGS Codegen &g, std::optional<Value> recever, const std::vector<Value> &args
	const auto &self = Type::Self::get_instance();
	declare_builtin(
		"@pointerTo",
		ctx.get_function_type(nullptr, self, { self }),
		[](ARGS) {
			auto type = args[0].get_type_value();
			return Value(g.ctx.get_pointer_type(type));
		}
	);
	declare_builtin(
		"@pointerTo",
		ctx.get_function_type(nullptr, self, { self, ctx.get_boolean_type() }),
		[](ARGS) {
			auto type = args[0].get_type_value();
			auto is_const = llvm::dyn_cast<llvm::Constant>(args[1].get_llvm_value());
			// TODO all one or?
			return Value(g.ctx.get_pointer_type(type, is_const->isAllOnesValue()));
		}
	);
#undef ARGS
}

llvm::Function *Codegen::get_llvm_function() const {
	if (auto block = get_builder()->GetInsertBlock())
		return block->getParent();
	else throw CodegenException("Top-level context doesn't have parent function");
}

void Codegen::add_layer(
	Ptr<llvm::IRBuilder<>> builder,
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

Value Codegen::allocate_stack(Type *type, bool is_const) {
	auto func = get_llvm_function();
	llvm::IRBuilder<> tmp_builder(ctx.get_llvm());
	auto &entry = func->getEntryBlock();
	tmp_builder.SetInsertPoint(&entry, entry.getFirstInsertionPt());
	return {
		ctx.get_pointer_type(type, is_const),
		tmp_builder.CreateAlloca(type->get_llvm(), 0, nullptr)
	};
}

Value Codegen::allocate_stack(Type *type, const Value &value, bool is_const) {
	auto ptr = allocate_stack(type, is_const);
	get_builder()->CreateStore(value.get_llvm_value(), ptr.get_llvm_value());
	return ptr;
}

} // namespace rin
