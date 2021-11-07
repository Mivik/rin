
#pragma once

#include "fmt/format.h"

#include <llvm/IR/IRBuilder.h>

#include "context.h"
#include "function.h"
#include "layer_map.h"

namespace rin {

class ASTNode;

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
	explicit Codegen(Codegen *parent);
	~Codegen();

	[[nodiscard]] Context &get_context() const { return ctx; }
	[[nodiscard]] llvm::LLVMContext &get_llvm_context() const { return ctx.get_llvm(); }
	[[nodiscard]] SPtr<llvm::Module> get_module() const { return module; }
	[[nodiscard]] bool is_inlined() const { return inline_depth; }
	void push_inline() { ++inline_depth; }
	void pop_inline() { --inline_depth; }
	Type::Function *get_function_type() const { return layers.back().function_type; }
	llvm::IRBuilder<> *get_builder() const { return layers.back().builder.get(); }

	[[nodiscard]] llvm::ConstantInt *get_constant_int(unsigned value) const {
		return llvm::ConstantInt::get(llvm::Type::getInt32Ty(get_llvm_context()), value);
	}

	[[nodiscard]] Value create_ref_value(Type::Ref *type, llvm::Value *llvm);
	[[nodiscard]] Value create_value(Type *type, llvm::Value *llvm) {
		if (auto ref_type = dynamic_cast<Type::Ref *>(type))
			return create_ref_value(ref_type, llvm);
		return { type, llvm };
	}

	template<class...Args>
	[[noreturn]] void error(const char *pattern, Args &&...args) const {
		throw CodegenException(fmt::format(pattern, std::forward<Args>(args)...));
	}

	[[nodiscard]] Type::Pointer *to_pointer_type(Type::Ref *ref) {
		return ctx.get_pointer_type(ref->get_sub_type(), ref->is_mutable());
	}

	[[nodiscard]] Type::Ref *to_ref_type(Type::Pointer *ptr) {
		return ctx.get_ref_type(ptr->get_sub_type(), ptr->is_mutable());
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
	T *declare_function(const std::string &name, Ptr<T> func) { // TODO conflict?
		static_assert(std::is_base_of_v<Function, T>);
		auto result = func.get();
		auto &vec = function_map.get_or_create(name);
		vec.push_back(std::move(func));
		return result;
	}

	template<class T, class...Args>
	T *create_ref(Args &&...args) {
		static_assert(std::is_base_of_v<Ref, T>);
		auto ptr = new T(std::forward<Args>(args)...);
		layers.back().refs.push_back(ptr);
		return ptr;
	}

	[[nodiscard]] llvm::Function *get_llvm_function() const;

	[[nodiscard]] SPtr<llvm::Module> finalize() { return std::move(module); }

	llvm::BasicBlock *create_basic_block(const std::string &name) const {
		return llvm::BasicBlock::Create(ctx.get_llvm(), name, get_llvm_function());
	}

	Value allocate_stack(Type *type, bool is_mutable);
	Value allocate_stack(Type *type, const Value &default_value, bool is_const);

	Function::Static *declare_function(Type::Function *type, const std::string &name, bool no_mangle);
	void implement_function(
		Function::Static *function,
		const std::vector<std::string> &parameter_names,
		ASTNode *content_node
	);
	void implement_function(
		Type::Function *type,
		const std::vector<std::string> &parameter_names,
		std::optional<Value> receiver,
		const std::vector<Value> &arguments,
		ASTNode *content_node
	);

	void create_return(Value value);

	void add_layer(Type::Function *function, SPtr<llvm::IRBuilder<>> builder = nullptr);
	void pop_layer();

	Codegen *derive_inline_context(Type *type, Value &result);
	void dispose_inline_context(Codegen *g);

	bool having_inline_call() { return inline_call_result; }
private:
	struct Layer {
		SPtr<llvm::IRBuilder<>> builder;
		Type::Function *function_type;
		std::vector<Ref *> refs;
	};

	void declare_builtin(
		const std::string &name,
		std::string type_desc,
		Function::Builtin::VerifierType verifier,
		Function::Builtin::FuncType func
	) {
		function_map
			.get_or_create("@" + name)
			.emplace_back(new Function::Builtin(std::move(type_desc), std::move(verifier), std::move(func)));
	}

	void init();

	Context &ctx;
	Codegen *parent;
	SPtr<llvm::Module> module;
	std::vector<Layer> layers;
	LayerMap<std::string, Value> value_map;
	LayerMap<std::string, std::vector<Ptr<Function>>> function_map;
	uint32_t inline_depth;
	llvm::PHINode *inline_call_result;
	llvm::BasicBlock *inline_call_dest{};
};

template<>
inline Function::Static *Codegen::declare_function(const std::string &name, Ptr<Function::Static> func) {
	auto result = func.get();
	auto type = func->get_type();
	auto &vec = function_map.get_or_create(name);
	for (const auto &element : vec) { // TODO optimize this!!!
		auto target = dynamic_cast<Function::Static *>(element.get());
		if (target && target->get_type() == type) return nullptr;
	}
	vec.push_back(std::move(func));
	return result;
}

} // namespace rin
