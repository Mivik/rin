
#include <gtest/gtest.h>

#include <functional>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/BasicBlock.h>

#include "parser.h"
#include "context.h"
#include "jit.h"

namespace rin {

TEST(exec, manual) {
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
	std::vector<llvm::Value *> args;
	for (auto &arg : func->args())
		args.push_back(&arg);

	builder.CreateRet(builder.CreateAdd(args[0], args[1]));

	JITEngine engine(std::move(module));

	auto add = engine.find_function<int, int, int>("add");
	EXPECT_EQ(add(5, 3), 8);
	EXPECT_EQ(add(-5, 10), 5);
}

TEST(exec, simple) {
	CoreContext core;
	Context ctx(core);
	Parser(R"(
		fn mul(a: i32, b: i32): i32 {
			return a * b
		}
	)").take_function()->codegen(ctx);

	JITEngine engine(ctx.finalize());
	auto add = engine.find_function<int, int, int>("mul");
	EXPECT_EQ(add(5, 3), 15);
	EXPECT_EQ(add(-5, 10), -50);
}

TEST(exec, variable) {
	CoreContext core;
	Context ctx(core);
	Parser(R"(
		fn grid_point_count(a: i32, b: i32): i32 {
			let a = a + 1
			let b = b + 1
			return a * b
		}
	)").take_function()->codegen(ctx);

	JITEngine engine(ctx.finalize());
	auto add = engine.find_function<int, int, int>("grid_point_count");
	EXPECT_EQ(add(5, 3), 24);
}

TEST(exec, const_variable) {
	CoreContext core;
	Context ctx(core);
	EXPECT_THROW(Parser(R"(
		fn test() {
			let a: i64 = 5
			a = 6
		}
	)").take_function()->codegen(ctx), CodegenException);
}

TEST(exec, if_stmt) {
	CoreContext core;
	Context ctx(core);
	Parser(R"(
		fn abs(x: i32): i32 {
			if (x < 0) return -x
			else return x
		}
	)").take_function()->codegen(ctx);
	Parser(R"(
		fn abs2(x: i32): i32 {
			return if (x < 0) -x else x
		}
	)").take_function()->codegen(ctx);

	 ctx.get_module()->print(llvm::errs(), nullptr);

	JITEngine engine(ctx.finalize());

	auto test_abs = [&](const std::function<int(int)> &func) {
		EXPECT_EQ(abs(0), 0);
		EXPECT_EQ(abs(5), 5);
		EXPECT_EQ(abs(-6), 6);
	};
	test_abs(engine.find_function<int, int>("abs"));
//	test_abs(engine.find_function<int, int>("abs2"));
}

} // namespace rin
