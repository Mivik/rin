
#include <gtest/gtest.h>

#include "parser.h"

namespace rin {

TEST(parser, basic) {
	Parser parser("1 + 2 * 6 * (1 + 3 / 2)");
	auto node = parser.take_expr();
	Context ctx;
	Codegen g(ctx);
	auto value = node->codegen(g);
	value.dump();
}

} // namespace rin
