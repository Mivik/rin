
#include <gtest/gtest.h>

#include "codegen.h"
#include "parser.h"

namespace rin {

::testing::AssertionResult is_a(Value value, Type *type) {
	if (value.get_type() != type)
		return ::testing::AssertionFailure()
			<< "Expected a value of type " << type->to_string() << ", "
			<< "got " << value.get_type()->to_string();
	return ::testing::AssertionSuccess();
}

TEST(parser, boolean_constant) {
	CoreContext core;
	Context ctx(core);
	EXPECT_TRUE(is_a(Parser("true").take_prim()->codegen(ctx),
					 core.get_boolean_type()));
	EXPECT_TRUE(is_a(Parser("false").take_prim()->codegen(ctx),
					 core.get_boolean_type()));
}

TEST(parser, integer_constant) {
	CoreContext core;
	Context ctx(core);
	EXPECT_TRUE(is_a(Parser("123").take_prim()->codegen(ctx),
					 core.get_i32_type()));
	EXPECT_TRUE(is_a(Parser("2333U").take_prim()->codegen(ctx),
					 core.get_u32_type()));
	EXPECT_TRUE(is_a(Parser("2147483648ll").take_prim()->codegen(ctx),
					 core.get_i64_type()));
	EXPECT_TRUE(is_a(Parser("9223372036854775810ULL").take_prim()->codegen(ctx),
					 core.get_u64_type()));
}

TEST(parser, source_range) {
	EXPECT_EQ(Parser("1+2").take_expr()->get_source_range(), SourceRange(0, 3));
	auto bin_op = ptr_cast<BinOpNode>(Parser(" 1+ 45 ").take_expr());
	EXPECT_TRUE(bin_op);
	EXPECT_EQ(bin_op->get_source_range(), SourceRange(1, 6));
	EXPECT_EQ(bin_op->get_lhs_node()->get_source_range(), SourceRange(1, 2));
	EXPECT_EQ(bin_op->get_rhs_node()->get_source_range(), SourceRange(4, 6));
}

TEST(parser, bin_op) {
	CoreContext core;
	Context ctx(core);
	EXPECT_TRUE(is_a(Parser("1 + 2U").take_expr()->codegen(ctx),
					 core.get_i32_type()));
	EXPECT_TRUE(is_a(Parser("1U << 2").take_expr()->codegen(ctx),
					 core.get_u32_type()));
	EXPECT_TRUE(is_a(Parser("1ULL + 2").take_expr()->codegen(ctx),
					 core.get_u64_type()));
	{
		auto value = Parser("(50 + (-3) * (~7)) / 4 % 5").take_expr()->codegen(ctx);
		EXPECT_EQ(value.get_type(), core.get_i32_type());
		auto llvm = value.get_llvm();
		auto const_int = llvm::dyn_cast<llvm::ConstantInt>(llvm);
		EXPECT_TRUE(const_int);
		EXPECT_EQ(const_int->getSExtValue(), 3);
	}
	{
		auto value = Parser("2 + 3 * 4 == 14").take_expr()->codegen(ctx);
		EXPECT_EQ(value.get_type(), core.get_boolean_type());
		auto llvm = value.get_llvm();
		auto const_int = llvm::dyn_cast<llvm::ConstantInt>(llvm);
		EXPECT_TRUE(const_int);
		EXPECT_EQ(const_int->getZExtValue(), 1);
	}
	{
		auto value = Parser("(2 == 3 - (3 ^ 2)) == !false").take_expr()->codegen(ctx);
		EXPECT_EQ(value.get_type(), core.get_boolean_type());
		auto llvm = value.get_llvm();
		auto const_int = llvm::dyn_cast<llvm::ConstantInt>(llvm);
		EXPECT_TRUE(const_int);
		EXPECT_EQ(const_int->getZExtValue(), 1);
	}
	EXPECT_THROW(Parser("(1 + 3").take_expr(), ParseException);
	EXPECT_THROW(Parser("1 !2").take_expr(), ParseException);
	EXPECT_THROW(Parser("2 +").take_expr(), ParseException);
	EXPECT_NO_FATAL_FAILURE(Parser("1 + -3 - -6").take_expr());
}

TEST(parser, type) {
	CoreContext core;
	Context ctx(core);
	EXPECT_EQ(Parser("i8").take_type()->codegen(ctx), core.get_i8_type());
	EXPECT_EQ(Parser("u128").take_type()->codegen(ctx), core.get_u128_type());
	EXPECT_EQ(Parser("*i8").take_type()->codegen(ctx), core.get_pointer_type(core.get_i8_type()));
	EXPECT_EQ(Parser("[&const i32, 54]").take_type()->codegen(ctx),
			  core.get_array_type(core.get_ref_type(core.get_i32_type(), true), 54));
	EXPECT_EQ(Parser("[[i32, 5], 5]").take_type()->codegen(ctx),
			  core.get_array_type(core.get_array_type(core.get_i32_type(), 5), 5));
	EXPECT_THROW(Parser("&(&i8)").take_type()->codegen(ctx), CodegenException);
	EXPECT_THROW(Parser("*(&i32)").take_type()->codegen(ctx), CodegenException);
}

TEST(parser, stmt) {
	EXPECT_THROW(Parser("let i;").take_stmt(), CodegenException);
	EXPECT_TRUE(dynamic_cast<VarDeclNode *>(
					Parser("let i: i32;").take_stmt().get()
				));
	EXPECT_TRUE(dynamic_cast<BlockNode *>(
					Parser(R"(
			{
				let i: i32 = 5;
				100;
			}
		)").take_stmt().get()
				));
	EXPECT_THROW(Parser("let v = 5").take_stmt(), ParseException);
}

TEST(parser, function) {
	CoreContext core;
	Context ctx(core);
	auto i8 = core.get_i8_type();
	EXPECT_EQ(Parser("(i8, i8) -> i8").take_type()->codegen(ctx),
			  core.get_function_type(nullptr, i8, {i8, i8}));
	EXPECT_EQ(Parser("i8.() -> bool").take_type()->codegen(ctx),
			  core.get_function_type(i8, core.get_boolean_type(), {}));
}

} // namespace rin
