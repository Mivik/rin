
#include <memory>

#include "ast.h"
#include "codegen.h"
#include "ref.h"

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
	declare_value("any", Value(ctx.get_any_concept())); // TODO remove this
	add_layer(std::make_unique<llvm::IRBuilder<>>(ctx.get_llvm()), nullptr);
}

Codegen::~Codegen() {
	while (!layers.empty()) pop_layer();
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
	Ptr <llvm::IRBuilder<>> builder,
	Function::Static *function
) {
	layers.push_back(
		{
			std::move(builder),
			function,
			{}
		}
	);
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

Function::Static *Codegen::declare_function(
	Type::Function *type,
	const std::string &name
) {
	auto llvm = llvm::Function::Create(
		llvm::dyn_cast<llvm::FunctionType>(
			type->get_llvm()
		),
		llvm::Function::ExternalLinkage,
		// TODO mangle
		name,
		module.get()
	);
	return declare_function(
		name,
		std::make_unique<Function::Static>(Value(type, llvm), false) // TODO const eval
	);
}

void Codegen::implement_function(
	Function::Static *function,
	const std::vector<std::string> &param_names,
	BlockNode *body_node
) {
	auto llvm = function->get_llvm_value();
	auto type = function->get_type();
	add_layer(
		std::make_unique<llvm::IRBuilder<>>(
			llvm::BasicBlock::Create(
				get_llvm_context(),
				"entry",
				llvm
			)
		),
		function
	);
	std::vector<llvm::Value *> args;
	for (auto &arg : llvm->args())
		args.push_back(&arg);
	auto receiver_type = type->get_receiver_type();
	const bool has_receiver = receiver_type;
	if (receiver_type)
		declare_value(
			"this",
			create_value(receiver_type, args[0])
		);
	const auto &param_types = type->get_parameter_types();
	for (size_t i = 0; i < param_types.size(); ++i)
		declare_value(param_names[i], create_value(param_types[i], args[i + has_receiver]));
	body_node->codegen(*this);
	if (!body_node->has_return()) {
		if (type->get_result_type() == ctx.get_void_type())
			get_builder()->CreateRetVoid();
		else error("Non-void function does not return a value");
	}
	pop_layer();
}

} // namespace rin
