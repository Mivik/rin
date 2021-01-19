
#pragma once

#include <llvm/IR/IRBuilder.h>

#include "core_context.h"
#include "layer_map.h"
#include "utility.h"

namespace rin {

class Function;

class Value;

class Context {
public:
	explicit Context(CoreContext &core, const std::string &name = "program");
	inline CoreContext &get_core() const { return core; }
	inline llvm::LLVMContext &get_llvm() const { return core.get_llvm(); }
	inline llvm::Module *get_module() const { return module.get(); }
	inline Function *get_function() const { return functions.back(); }
	inline llvm::IRBuilder<> &get_builder() const { return *builders.back(); }
	llvm::Function *get_llvm_function() const;
	std::optional<Value> lookup_value(const std::string &name) const;
	std::optional<Type *> lookup_type(const std::string &name) const;
	std::optional<Function *> lookup_function(const std::string &name) const;
	void declare_value(const std::string &name, const Value &value);
	void declare_type(const std::string &name, Type *type);
	void declare_function(const std::string &name, Function *func);
	Value allocate_stack(Type *type, bool is_const);
	Value allocate_stack(Type *type, const Value &default_value, bool is_const);
	void add_layer(std::unique_ptr<llvm::IRBuilder<>> builder, Function *function);
	void pop_layer();

	llvm::BasicBlock *create_basic_block(const std::string &name) const;

	std::unique_ptr<llvm::Module> finalize();

	DISABLE_COPY(Context)
private:
	CoreContext &core;
	std::unique_ptr<llvm::Module> module;
	std::vector<Ptr<llvm::IRBuilder<>>> builders;
	std::vector<Function *> functions;
	LayerMap<std::string, Value> value_map;
	LayerMap<std::string, Type *> type_map;
	LayerMap<std::string, Function *> function_map;
};

} // namespace rin
