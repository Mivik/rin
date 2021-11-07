
#include <memory>

#include "ast.h"
#include "codegen.h"
#include "ref.h"

namespace rin {

Codegen::Codegen(Context &ctx, const std::string &name):
	ctx(ctx), parent(nullptr),
	module(std::make_unique<llvm::Module>(name, ctx.get_llvm())),
	inline_depth(0), inline_call_result(nullptr), inline_call_dest(nullptr) {
	init();
}

Codegen::Codegen(Codegen *parent):
	ctx(parent->get_context()), parent(parent),
	module(parent->get_module()),
	inline_depth(0), inline_call_result(nullptr), inline_call_dest(nullptr) {
	init();
}

Codegen::~Codegen() {
	while (!layers.empty()) pop_layer();
}

void Codegen::init() {
	add_layer(nullptr);
#define ARGS Codegen &g, std::optional<Value> receiver, const std::vector<Value> &args
	// TODO builtin functions here
	declare_builtin("address", "fn (&T): *T", [](ARGS) {
		return args.size() == 1 && args[0].is_ref_value() && dynamic_cast<Ref::Address *>(args[0].get_ref_value());
	}, [&](ARGS) {
		auto ref = args[0].get_ref_value();
		auto type = ref->get_type();
		return Value(
			to_pointer_type(type),
			dynamic_cast<Ref::Address *>(ref)->get_address()
		);
	});
	declare_builtin("type", "fn (T): type", [](ARGS) {
		return args.size() == 1;
	}, [](ARGS) {
		return Value(args[0].get_type());
	});
	declare_builtin("cast", "fn [Int].(T): T", [](ARGS) {
		return receiver
			   && dynamic_cast<Type::Int *>(receiver->get_type())
			   && args.size() == 1 && args[0].is_type_value() && dynamic_cast<Type::Int *>(args[0].get_type_value());
	}, [](ARGS) {
		auto dest = args[0].get_type_value();
		return g.create_value(
			dest,
			g.get_builder()->CreateZExtOrTrunc(receiver->get_llvm_value(), dest->get_llvm())
		);
	});
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
	declare_value("any", Value(ctx.get_any_concept())); // TODO remove this
	add_layer(nullptr);
}

Value Codegen::create_ref_value(Type::Ref *type, llvm::Value *llvm) {
	return Value(create_ref<Ref::Address>(type, llvm));
}

llvm::Function *Codegen::get_llvm_function() const {
	if (auto block = get_builder()->GetInsertBlock())
		return block->getParent();
	else throw CodegenException("Top-level context doesn't have parent function");
}

void Codegen::add_layer(
	Type::Function *function,
	SPtr<llvm::IRBuilder<>> builder
) {
	if (!builder) builder = std::make_shared<llvm::IRBuilder<>>(ctx.get_llvm());
	layers.push_back(
		{
			std::move(builder),
			function,
			{}
		}
	);
	value_map.add_layer();
}

void Codegen::pop_layer() {
	for (auto ref : layers.back().refs)
		delete ref;
	layers.pop_back();
	value_map.pop_layer();
}

Value Codegen::allocate_stack(Type *type, bool is_mutable) {
	type = type->deref();
	auto func = get_llvm_function();
	llvm::IRBuilder<> tmp_builder(ctx.get_llvm());
	auto &entry = func->getEntryBlock();
	tmp_builder.SetInsertPoint(&entry, entry.getFirstInsertionPt());
	return {
		ctx.get_pointer_type(type, is_mutable),
		tmp_builder.CreateAlloca(type->get_llvm(), 0, nullptr)
	};
}

Value Codegen::allocate_stack(Type *type, const Value &value, bool is_const) {
	auto ptr = allocate_stack(type, is_const);
	get_builder()->CreateStore(value.get_llvm_value(), ptr.get_llvm_value());
	return ptr;
}

Function::Static *Codegen::declare_function(
	Type::Function *type,
	const std::string &name,
	bool no_mangle
) {
	auto llvm = llvm::Function::Create(
		llvm::dyn_cast<llvm::FunctionType>(type->get_llvm()),
		llvm::Function::ExternalLinkage,
		no_mangle? name: type->to_string(name, true),
		module.get()
	);
	return declare_function(
		name,
		std::make_unique<Function::Static>(Value(type, llvm)) // TODO const eval
	);
}

void Codegen::implement_function(
	Function::Static *function,
	const std::vector<std::string> &parameter_names,
	ASTNode *content_node
) {
	auto llvm = function->get_llvm_value();
	auto type = function->get_type();
	add_layer(
		function->get_type(),
		std::make_unique<llvm::IRBuilder<>>(
			llvm::BasicBlock::Create(
				get_llvm_context(),
				"entry",
				llvm
			)
		)
	);
	auto param_types = type->get_parameter_types();
	auto receiver_type = type->get_receiver_type();
	std::optional<Value> receiver = std::nullopt;
	std::vector<Value> args;
	args.reserve(param_types.size());
	size_t index = -(receiver_type != nullptr);
	for (auto &arg : llvm->args()) {
		if (index == -1) receiver = create_value(receiver_type, &arg);
		else args.push_back(create_value(param_types[index], &arg));
		++index;
	}
	implement_function(type, parameter_names, receiver, args, content_node);
	pop_layer();
}

void Codegen::implement_function(
	Type::Function *type,
	const std::vector<std::string> &parameter_names,
	std::optional<Value> receiver,
	const std::vector<Value> &arguments,
	ASTNode *content_node
) {
	if (receiver) declare_value("this", receiver.value());
	for (size_t i = 0; i < arguments.size(); ++i)
		declare_value(parameter_names[i], arguments[i]);
	if (auto body_node = dynamic_cast<BlockNode *>(content_node)) {
		body_node->codegen(*this);
		if (!body_node->has_return()) {
			if (type->get_result_type() == ctx.get_void_type())
				create_return(ctx.get_void());
			else error("Non-void function does not return a value");
		}
	} else {
		auto result = content_node->codegen(*this);
		if (result.get_type() != type->get_result_type())
			error(
				"Returning a {} in a function that returns {}",
				result.get_type()->to_string(), type->get_result_type()->to_string()
			);
		create_return(result);
	}
}

void Codegen::create_return(Value value) {
	if (parent) {
		assert(parent->inline_call_result);
		parent->inline_call_result->addIncoming(
			value.get_llvm_value(),
			get_builder()->GetInsertBlock()
		);
		get_builder()->CreateBr(parent->inline_call_dest);
	} else if (value.get_type() == ctx.get_void_type())
		get_builder()->CreateRetVoid();
	else get_builder()->CreateRet(value.get_llvm_value());
}

Codegen *Codegen::derive_inline_context(Type *type, Value &result) {
	assert(!having_inline_call());
	inline_call_dest = llvm::BasicBlock::Create(
		ctx.get_llvm(), "sub_merge", get_builder()->GetInsertBlock()->getParent()
	);
	get_builder()->SetInsertPoint(inline_call_dest);
	inline_call_result = get_builder()->CreatePHI(type->get_llvm(), 0);
	result = { type, inline_call_result };
	return new Codegen(this);
}

void Codegen::dispose_inline_context(Codegen *g) {
	delete g;
	inline_call_result = nullptr;
	inline_call_dest = nullptr;
}

Function::Static *Codegen::find_function(const std::string &name, Type::Function *type) {
	auto iter = function_map.find(name);
	if (iter == function_map.end()) return nullptr;
	for (const auto &element : iter->second) { // TODO optimize this!!!
		auto target = dynamic_cast<Function::Static *>(element.get());
		if (target && target->get_type() == type) return target;
	}
	return nullptr;
}

} // namespace rin
