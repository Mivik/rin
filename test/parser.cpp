
#include <gtest/gtest.h>

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

TEST(parser, bin_op) {
	CoreContext core;
	Context ctx(core);
	EXPECT_TRUE(is_a(Parser("1 + 2U").take_expr()->codegen(ctx),
				core.get_i32_type()));
	EXPECT_TRUE(is_a(Parser("1U << 2").take_expr()->codegen(ctx),
				core.get_u32_type()));
	EXPECT_TRUE(is_a(Parser("1ULL + 2").take_expr()->codegen(ctx),
				core.get_u64_type()));
	do {
		auto value = Parser("(50 + (-3) * (~7)) / 4 % 5").take_expr()->codegen(ctx);
		EXPECT_EQ(value.get_type(), core.get_i32_type());
		auto llvm = value.get_llvm();
		auto const_int = llvm::dyn_cast<llvm::ConstantInt>(llvm);
		EXPECT_TRUE(const_int);
		EXPECT_EQ(const_int->getSExtValue(), 3);
	} while (false);
	do {
		auto value = Parser("2 + 3 * 4 == 14").take_expr()->codegen(ctx);
		EXPECT_EQ(value.get_type(), core.get_boolean_type());
		auto llvm = value.get_llvm();
		auto const_int = llvm::dyn_cast<llvm::ConstantInt>(llvm);
		EXPECT_TRUE(const_int);
		EXPECT_EQ(const_int->getZExtValue(), 1);
	} while (false);
	do {
		auto value = Parser("(2 == 3 - (3 ^ 2)) == !false").take_expr()->codegen(ctx);
		EXPECT_EQ(value.get_type(), core.get_boolean_type());
		auto llvm = value.get_llvm();
		auto const_int = llvm::dyn_cast<llvm::ConstantInt>(llvm);
		EXPECT_TRUE(const_int);
		EXPECT_EQ(const_int->getZExtValue(), 1);
	} while (false);
	EXPECT_THROW(Parser("(1 + 3").take_expr(), ParseException);
	EXPECT_NO_FATAL_FAILURE(Parser("1 + -3 - -6").take_expr());
}

} // namespace rin
