
#include <gtest/gtest.h>

#include "parser.h"

namespace rin {

template<class T>
inline void expect_int(Value value, T expect) {
	static_assert(std::is_integral_v<T>);
	auto constant = llvm::dyn_cast<llvm::ConstantInt>(value.get_llvm_value());
	EXPECT_TRUE(constant);
	EXPECT_EQ(constant->getUniqueInteger(), expect);
}

TEST(parser, basic) {
	Context ctx;
	Codegen g(ctx);

	auto eval = [&](const char *code) { return Parser(code).take_expr()->codegen(g); };
	expect_int(eval("1 + 2 * 6 * (1 + 3 / 2)"), 25);
	expect_int(eval("2 ^ (15 & 39)"), 5);
	expect_int(eval("998244353L * 998244353L"), 996491788296388609L);
	expect_int(eval("2147483647U + 1U"), 2147483648U);
	expect_int(eval("if (true) 2 else 1"), 2);
}

} // namespace rin
