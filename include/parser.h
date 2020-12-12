
#pragma once

#include "ast.h"
#include "lexer.h"

namespace rin {

class ParseException : public std::exception {
public:
	const char *what() const noexcept { return msg.data(); }
private:
	std::string msg;
	ParseException(const std::string &msg): msg(msg) {}

	friend class Parser;
};

class Parser {
public:
	Parser(const char *str): Parser(MemoryBuffer(str)) {}
	Parser(const MemoryBuffer &buffer): lexer(buffer) {}

	Ptr<ASTNode<Ptr<Value*>>> take_prim();
	inline const MemoryBuffer& get_buffer() const { return lexer.get_buffer(); }
private:
	inline const Token&& expect(const Token &&token, TokenKind kind) {
		if (token.kind != kind)
			throw ParseException(
				"Expected " + token_kind_name(kind) + ", "
				+ "got " + token.name()
				+ " (" + token.content(lexer.get_buffer()) + ")");
		return std::move(token);
	}

	Lexer lexer;
};

} // namespace rin
