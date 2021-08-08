
#pragma once

#include "lexer.h"

namespace rin {

class Parser {
public:
	explicit Parser(const char *str): Parser(std::string_view(str)) {}
	explicit Parser(const std::string_view &input): lexer(input) {}

	Lexer lexer;
};

} // namespace rin
