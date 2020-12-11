
#include "lexer.h"
#include "token.h"

namespace rin {

std::string Token::content(const MemoryBuffer &buffer) const {
	return buffer.substr(range);
}

} // namespace rin
