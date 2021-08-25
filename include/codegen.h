
#pragma once

#include "fmt/format.h"

#include <llvm/IR/IRBuilder.h>

#include "context.h"
#include "function.h"
#include "layer_map.h"

namespace rin {

class Function;

class CodegenException : public std::exception {
public:
	[[nodiscard]] const char *what() const noexcept override { return msg.data(); }
private:
	explicit CodegenException(std::string msg): msg(std::move(msg)) {}

	std::string msg;

	friend class Codegen;
};

class Codegen {
public:
	explicit Codegen(Context &ctx, const std::string &name = "program");
	[[nodiscard]] Context &get_context() const { return ctx; }
	[[nodiscard]] llvm::LLVMContext &get_llvm_context() const { return ctx.get_llvm(); }
	[[nodiscard]] llvm::Module *get_module() const { return module.get(); }
	[[nodiscard]] bool is_const_eval() const { return const_eval_depth; }
	void push_const_eval() { ++const_eval_depth; }
	void pop_const_eval() { --const_eval_depth; }
	Function::Static *get_function() const { return layers.back().function; }
	llvm::IRBuilder<> *get_builder() const { return layers.back().builder.get(); }

	template<class...Args>
	[[noreturn]] void error(const char *pattern, Args&&...args) const {
		throw CodegenException(fmt::format(pattern, std::forward<Args>(args)...));
	}

	void declare_value(const std::string &name, const Value &value) {
		value_map.set(name, value);
	}
	[[nodiscard]] std::optional<Value> lookup_value(const std::string &name) const {
		return value_map.try_get(name);
	}

	[[nodiscard]] bool has_function(const std::string &name) const { return function_map.has(name); }
	[[nodiscard]] const std::vector<std::vector<Ptr<Function>>> &lookup_functions(const std::string &name) const {
		return function_map.get_all(name);
	}

	template<class T>
	T *declare_function(const std::string &name, Ptr<T> func) {
		static_assert(std::is_base_of_v<Function, T>);
		auto result = func.get();
		function_map.get_or_create(name).push_back(std::move(func));
		return result;
	}

	[[nodiscard]] llvm::Function *get_llvm_function() const;

	[[nodiscard]] Ptr<llvm::Module> finalize() { return std::move(module); }

	llvm::BasicBlock *create_basic_block(const std::string &name) const {
		return llvm::BasicBlock::Create(ctx.get_llvm(), name, get_llvm_function());
	}

	Value allocate_stack(Type *type, bool is_const);
	Value allocate_stack(Type *type, const Value &default_value, bool is_const);

	void add_layer(Ptr<llvm::IRBuilder<>> builder, Function::Static *function);
	void pop_layer();
private:
	struct Layer {
		Ptr<llvm::IRBuilder<>> builder;
		Function::Static *function;
	};

	void declare_builtin(const std::string &name, Type::Function *type, Function::Builtin::FuncType func) {
		function_map.get_or_create(name).emplace_back(new Function::Builtin(type, std::move(func)));
	}

	Context &ctx;
	Ptr<llvm::Module> module;
	std::vector<Layer> layers;
	LayerMap<std::string, Value> value_map;
	LayerMap<std::string, std::vector<Ptr<Function>>> function_map;
	uint32_t const_eval_depth;
};

} // namespace rin
