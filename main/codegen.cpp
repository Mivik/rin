
#include <memory>

#include "codegen.h"

namespace rin {

Codegen::Codegen(Context &ctx, const std::string &name):
	ctx(ctx),
	module(std::make_unique<llvm::Module>(name, ctx.get_llvm())),
	const_eval_depth(0) {
	add_layer(std::make_unique<llvm::IRBuilder<>>(ctx.get_llvm()), nullptr);
#define ARGS Codegen &g, std::optional<Value> recever, const std::vector<Value> &args
	// TODO builtin functions here
#undef ARGS
	// TODO more integer types
#define TYPE(n) declare_value(#n, Value(ctx.get_##n##_type()));
	TYPE(i8)
	TYPE(i16)
	TYPE(i32)
	TYPE(i64)
	TYPE(i128)
	TYPE(u8)
	TYPE(u16)
	TYPE(u32)
	TYPE(u64)
	TYPE(u128)
	TYPE(float)
	TYPE(double)
#undef TYPE
	declare_value("bool", Value(ctx.get_boolean_type()));
	declare_value("void", Value(ctx.get_void_type()));
	declare_value("type", Value(Type::Self::get_instance()));
	add_layer(std::make_unique<llvm::IRBuilder<>>(ctx.get_llvm()), nullptr);
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
	for (auto ref : layers.back().refs)
		delete ref;
	layers.pop_back();
	value_map.pop_layer();
	function_map.pop_layer();
}

Value Codegen::allocate_stack(Type *type, bool is_const) {
	type = type->deref();
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
