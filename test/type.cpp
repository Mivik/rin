
#include <gtest/gtest.h>

#include <llvm/IR/Constants.h>

#include "core_context.h"

namespace rin {

TEST(type, integer) {
	CoreContext ctx;
	ASSERT_EQ(ctx.get_i8_type(), ctx.get_int_type(8, true));
	ASSERT_EQ(ctx.get_u128_type(), ctx.get_int_type(128, false));
	ASSERT_EQ(ctx.get_int_type(42, true), ctx.get_int_type(42, true));
	ASSERT_EQ(ctx.get_u64_type()->get_bit_width(), 64);
	ASSERT_TRUE(ctx.get_i128_type()->is_signed());
	ASSERT_FALSE(ctx.get_u32_type()->is_signed());
}

TEST(type, array) {
	CoreContext ctx;
	const auto i8 = ctx.get_i8_type();
	ASSERT_EQ(ctx.get_array_type(i8, 54), ctx.get_array_type(i8, 54));
	ASSERT_EQ(ctx.get_array_type(ctx.get_array_type(i8, 5), 5),
				ctx.get_array_type(ctx.get_array_type(i8, 5), 5));
}

} // namespace rin
