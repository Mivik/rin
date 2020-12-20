
#include <gtest/gtest.h>

#include "utility.h"

namespace rin {

TEST(utility, add_indent) {
	EXPECT_EQ(
		add_indent(
			"let i = 32\n"
			"calc(i)"
		),
		"\tlet i = 32\n"
		"\tcalc(i)"
	);
}

} // namespace rin
