
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

	JITEngine engine(ctx.finalize());

	auto test_abs = [&](const std::string &name) {
		const auto fund = engine.find_function<int, int>(name);
		EXPECT_EQ(abs(0), 0);
		EXPECT_EQ(abs(5), 5);
		EXPECT_EQ(abs(-6), 6);
	};
	test_abs("abs");
	test_abs("abs2");
}

TEST(exec, while_stmt) {
	CoreContext core;
	Context ctx(core);
	Parser(R"(
		fn factorial(n: i32): i32 {
			let* i = n
			let* ret = 1
			while (i != 0) {
				ret *= i
				i -= 1
			}
			return ret
		}
	)").take_function()->codegen(ctx);
	Parser(R"(
		fn factorial2(n: i32): i32 {
			let* i = n
			let* ret = 1
			do {
				ret *= i
				i -= 1
			} while (i != 0)
			return ret
		}
	)").take_function()->codegen(ctx);

	JITEngine engine(ctx.finalize());

	const auto test_fac = [&](const std::string &name) {
		const auto func = engine.find_function<int, int>(name);
		EXPECT_EQ(func(1), 1);
		EXPECT_EQ(func(5), 120);
	};
	test_fac("factorial");
	test_fac("factorial2");
}

TEST(exec, call) {
	CoreContext core;
	Context ctx(core);
	Parser(R"(
		fn reciprocal(n: double): double {
			return 1 / n
		}
	)").take_function()->codegen(ctx);
	Parser(R"(
		fn harmonic(n: i32): double {
			let* ret: double = 0
			let* i = 1
			while (i <= n) {
				ret += reciprocal(i)
				i += 1
			}
			return ret
		}
	)").take_function()->codegen(ctx);

	JITEngine engine(ctx.finalize());

	const auto error = 1e-10;
	const auto harmonic = engine.find_function<double, int>("harmonic");
	EXPECT_NEAR(harmonic(5), 137. / 60, error);
}

TEST(exec, explicit_casts) {
	CoreContext core;
	Context ctx(core);
	EXPECT_THROW(Parser(R"(
		fn t1() {
			let real: double = 50
			let int: i32 = real
		}
	)").take_function()->codegen(ctx), CastException);
	Parser(R"(
		fn t2() {
			let /*with comment*/int: i32 = 50
			let real: double = int
		}
	)").take_function()->codegen(ctx);
}

} // namespace rin
