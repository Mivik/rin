
#include <cassert>
#include <cctype>
#include <memory>

#include "parser.h"

namespace rin {

Ptr<ASTNode<Ptr<Value*>>> Parser::take_prim() {
	switch (lexer.peek().kind) {
		default: break;
	}
}

} // namespace rin
