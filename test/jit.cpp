
#include <gtest/gtest.h>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/BasicBlock.h>

#include "parser.h"
#include "context.h"
#include "jit.h"

namespace rin {

TEST(jit, manual) {
	llvm::LLVMContext ctx;
	auto module = std::make_unique<llvm::Module>("program", ctx);
	auto i32 = llvm::Type::getInt32Ty(ctx);
	auto type = llvm::FunctionType::get(
		i32,
		{ i32, i32 },
		false
	);
	auto func = llvm::Function::Create(type, llvm::Function::ExternalLinkage, "add", module.get());
	const auto block = llvm::BasicBlock::Create(ctx, "entry", func);
	llvm::IRBuilder<> builder(ctx);
	builder.SetInsertPoint(block);
	std::vector<llvm::Value*> args;
	for (auto &arg : func->args())
		args.push_back(&arg);

	builder.CreateRet(builder.CreateAdd(args[0], args[1]));

	JITEngine engine(std::move(module));

	auto add = engine.find_function<int, int, int>("add");
	EXPECT_EQ(add(5, 3), 8);
	EXPECT_EQ(add(-5, 10), 5);
}

TEST(jit, simple) {
	CoreContext core;
	Context ctx(core);
	Parser(R"(
		fn mul(a: i32, b: i32): i32 {
			return a * b;
		}
	)").take_function()->codegen(ctx);

	ctx.get_module()->print(llvm::outs(), nullptr);

	JITEngine engine(ctx.finalize());
	auto add = engine.find_function<int, int, int>("mul");
	EXPECT_EQ(add(5, 3), 15);
	EXPECT_EQ(add(-5, 10), -50);
}

} // namespace rin
