
#include <cassert>

#include "lexer.h"
#include "token.h"

namespace rin {

std::string_view Token::content(std::string_view buffer) const {
	return buffer.substr(range.begin, range.end - range.begin);
}

namespace token_kind {

std::string name(TokenKind kind) {
	switch (kind) {
#define TOKEN(name) case TokenKind::name: return #name;

#include "token.def"
		default: assert(false);
	}
}

} // namespace token_kind

} // namespace rin
