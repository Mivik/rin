
#pragma once

#include <llvm/IR/IRBuilder.h>

#include "core_context.h"
#include "layer_map.h"
#include "utility.h"

namespace rin {

class Value;

class Context {
public:
	Context(CoreContext &core, const std::string &name = "program");
	inline CoreContext& get_core() { return core; }
	inline llvm::LLVMContext& get_llvm() { return core.get_llvm(); }
	inline llvm::Module* get_module() { return module.get(); }
	inline llvm::IRBuilder<>& get_builder() { return builder; }
	std::optional<Value> lookup_value(const std::string &name) const;
	std::optional<Type*> lookup_type(const std::string &name) const;
	void declare_value(const std::string &name, const Value &value);
	void declare_type(const std::string &name, Type *type);
	Value allocate_stack(Type *type);
	Value allocate_stack(Type *type, const Value &default_value);
	Context sub_context(const llvm::IRBuilder<> &builder) const;

	DISABLE_COPY(Context)

	~Context();
private:
	Context(
		CoreContext &core,
		const std::shared_ptr<llvm::Module> module,
		const llvm::IRBuilder<> &builder,
		const std::shared_ptr<LayerMap<std::string, Value>> &value_map,
		const std::shared_ptr<LayerMap<std::string, Type*>> &type_map
	): core(core), module(module), builder(builder),
		value_map(value_map), type_map(type_map) {}

	CoreContext &core;
	std::shared_ptr<llvm::Module> module;
	llvm::IRBuilder<> builder;
	std::shared_ptr<LayerMap<std::string, Value>> value_map;
	std::shared_ptr<LayerMap<std::string, Type*>> type_map;
};

} // namespace rin
