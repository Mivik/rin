
#include <gtest/gtest.h>

#include <llvm/IR/Constants.h>

#include "core_context.h"

namespace rin {

TEST(type, integer) {
	CoreContext core;
	EXPECT_EQ(core.get_i8_type(), core.get_int_type(8, true));
	EXPECT_EQ(core.get_u128_type(), core.get_int_type(128, false));
	EXPECT_EQ(core.get_int_type(42, true), core.get_int_type(42, true));
	EXPECT_EQ(core.get_u64_type()->get_bit_width(), 64);
	EXPECT_TRUE(core.get_i128_type()->is_signed());
	EXPECT_FALSE(core.get_u32_type()->is_signed());
}

TEST(type, array) {
	CoreContext core;
	const auto i8 = core.get_i8_type();
	EXPECT_EQ(core.get_array_type(i8, 54), core.get_array_type(i8, 54));
	EXPECT_EQ(core.get_array_type(core.get_array_type(i8, 5), 5),
				core.get_array_type(core.get_array_type(i8, 5), 5));
}

TEST(type, to_string) {
	CoreContext core;
	EXPECT_EQ(core.get_i8_type()->to_string(), "i8");
	EXPECT_EQ(core.get_u128_type()->to_string(), "u128");
	EXPECT_EQ(core.get_int_type(114, false)->to_string(), "u114");
	EXPECT_EQ(core.get_void_type()->to_string(), "void");
	EXPECT_EQ(core.get_boolean_type()->to_string(), "bool");
	EXPECT_EQ(core.get_float_type()->to_string(), "float");
	EXPECT_EQ(core.get_double_type()->to_string(), "double");
	EXPECT_EQ(core.get_array_type(core.get_i8_type(), 54)->to_string(), "[i8, 54]");
}

} // namespace rin
