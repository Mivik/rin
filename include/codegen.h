
#pragma once

#include <llvm/IR/IRBuilder.h>

#include "context.h"
#include "function.h"
#include "layer_map.h"

namespace rin {

class Function;

class CodegenException : public std::exception {
public:
	[[nodiscard]] const char *what() const noexcept override { return msg.data(); }

	explicit CodegenException(std::string msg): msg(std::move(msg)) {}
private:
	std::string msg;
};

class Codegen {
public:
	explicit Codegen(Context &ctx, const std::string &name = "program");
	[[nodiscard]] Context &get_context() const { return ctx; }
	[[nodiscard]] llvm::LLVMContext &get_llvm_context() const { return ctx.get_llvm(); }
	[[nodiscard]] llvm::Module *get_module() const { return module.get(); }
	Function::Static *get_function() const { return layers.back().function; }
	llvm::IRBuilder<> *get_builder() const { return layers.back().builder.get(); }

	void declare_value(const std::string &name, const Value &value) {
		value_map.set(name, value);
	}
	std::optional<Value> lookup_value(const std::string &name) const {
		return value_map.try_get(name);
	}

	void declare_function(const std::string &name, std::unique_ptr<Function> func) {
		function_map[name].push_back(std::move(func));
	}

	[[nodiscard]] llvm::Function *get_llvm_function() const;

	[[nodiscard]] std::unique_ptr<llvm::Module> finalize() { return std::move(module); }

	llvm::BasicBlock *create_basic_block(const std::string &name) const {
		return llvm::BasicBlock::Create(ctx.get_llvm(), name, get_llvm_function());
	}

	void add_layer(std::unique_ptr<llvm::IRBuilder<>> builder, Function::Static *function);
	void pop_layer();
private:
	struct Layer {
		std::unique_ptr<llvm::IRBuilder<>> builder;
		Function::Static *function;
	};

	Context &ctx;
	std::unique_ptr<llvm::Module> module;
	std::vector<Layer> layers;
	LayerMap<std::string, Value> value_map;
	LayerMap<std::string, std::vector<std::unique_ptr<Function>>> function_map;
};

} // namespace rin
