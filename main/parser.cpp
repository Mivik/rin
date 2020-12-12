
#include <cassert>
#include <cctype>
#include <memory>

#include "parser.h"

namespace rin {

Ptr<ASTNode<Value>> Parser::take_prim() {
	auto token = lexer.take();
	auto str = token.content(get_buffer());
	assert(!str.empty());
	switch (token.kind) {
		case Number:
		case True:
		case False:
			return std::make_unique<ConstantNode>(str);
		default: break;
	}
	throw ParseException("Unknown token: " + token.info(get_buffer()));
}

} // namespace rin
