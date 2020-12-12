
#include <gtest/gtest.h>

#include "parser.h"

namespace rin {

TEST(parser, boolean_constant) {
	CoreContext core;
	Context ctx(core);
	EXPECT_EQ(Parser("true").take_prim()->codegen(ctx).get_type(),
				core.get_boolean_type());
	EXPECT_EQ(Parser("false").take_prim()->codegen(ctx).get_type(),
				core.get_boolean_type());
}

TEST(parser, integer_constant) {
	CoreContext core;
	Context ctx(core);
	EXPECT_EQ(Parser("123").take_prim()->codegen(ctx).get_type(),
				core.get_i32_type());
	EXPECT_EQ(Parser("2147483648ll").take_prim()->codegen(ctx).get_type(),
				core.get_i64_type());
	EXPECT_EQ(Parser("9223372036854775810ULL").take_prim()->codegen(ctx).get_type(),
				core.get_u64_type());
}

} // namespace rin
