
#include "codegen.h"
#include "context.h"
#include "value.h"

namespace rin {

Context::Context(CoreContext &core, const std::string &name):
	core(core), module(std::make_shared<llvm::Module>(name, core.get_llvm())),
	builder(core.get_llvm()),
	value_map(std::make_shared<decltype(value_map)::element_type>()),
	type_map(std::make_shared<decltype(type_map)::element_type>()) {
#define P declare_type
	P("i8", core.get_i8_type()); P("i16", core.get_i16_type());
	P("i32", core.get_i32_type()); P("i64", core.get_i64_type());
	P("i128", core.get_i128_type());

	P("u8", core.get_u8_type()); P("u16", core.get_u16_type());
	P("u32", core.get_u32_type()); P("u64", core.get_u64_type());
	P("u128", core.get_u128_type());

	P("bool", core.get_boolean_type());
	P("float", core.get_float_type());
	P("double", core.get_double_type());
	P("void", core.get_void_type());
#undef P
}

Context::~Context() {
	value_map->pop_layer();
	type_map->pop_layer();
}

Context Context::sub_context(const llvm::IRBuilder<> &builder) const {
	value_map->add_layer();
	type_map->add_layer();
	return Context(
		core,
		module,
		builder,
		value_map,
		type_map
	);
}

std::optional<Value> Context::lookup_value(const std::string &name) const {
	return value_map->try_get(name);
}

std::optional<Type*> Context::lookup_type(const std::string &name) const {
	return type_map->try_get(name);
}

void Context::declare_value(const std::string &name, const Value &value) {
	value_map->set(name, value);
}

void Context::declare_type(const std::string &name, Type *type) {
	type_map->set(name, type);
}

Value Context::allocate_stack(Type *type) {
	auto block = builder.GetInsertBlock();
	if (!block)
		throw CodegenException("Attempt to allocate in top-level context");
	auto func = block->getParent();
	assert(func);
	llvm::IRBuilder<> tmp_builder(core.get_llvm());
	tmp_builder.SetInsertPoint(&func->getEntryBlock());
	return {
		core.get_pointer_type(type),
		tmp_builder.CreateAlloca(type->get_llvm(), 0, nullptr)
	};
}

Value Context::allocate_stack(Type *type, const Value &value) {
	auto ptr = allocate_stack(type);
	builder.CreateStore(ptr.get_llvm(), value.get_llvm());
	return ptr;
}

} // namespace rin
