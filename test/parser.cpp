
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
	expect_int(eval("-2 + 5"), 3);
	EXPECT_THROW(eval("1 + )"), ParseException);

	Parser(R"(
	fn main(): i32 {
		const Point = struct {
			x: i32,
			y: i32
		};
		const Rect = struct {
			lt: Point,
			rb: Point
		};
		val lt = Point{2, 3};
		val rb = Point{4, 5};
		val rect = Rect{
			lt, rb
		};
		return rect.lt.x;
	}
	)").take_function();
}

} // namespace rin
