
#pragma once

#include "ast.h"
#include "lexer.h"

namespace rin {

class ParseException : public std::exception {
public:
	[[nodiscard]] const char *what() const noexcept override { return msg.data(); }

	explicit ParseException(std::string msg): msg(std::move(msg)) {}
private:
	std::string msg;
};

class Parser {
public:
	explicit Parser(const char *str): Parser(std::string_view(str)) {}
	explicit Parser(const std::string_view &input): lexer(input) {}

	[[nodiscard]] std::string_view get_buffer() const { return lexer.get_buffer(); }
	[[nodiscard]] const Reader &get_reader() const { return lexer.get_reader(); }

	Ptr<ASTNode> take_prim();
	Ptr<ASTNode> take_expr();
private:
	Token expect(const Token &&token, TokenKind kind) const {
		if (token.kind != kind) // TODO unlikely
			throw ParseException(
				"Expected " + token_kind::name(kind) + ", "
				+ "got " + token.info(get_buffer())
			);
		return token;
	}

	template<class Func>
	void process_list(
		TokenKind begin, TokenKind end, TokenKind delimiter,
		const Func &func
	) {
		expect(lexer.take(), begin);
		if (lexer.peek().kind == end) {
			lexer.take();
			return;
		}
		while (true) {
			func();
			auto token = lexer.take();
			if (token.kind == delimiter) continue;
			if (token.kind == end) break;
			throw ParseException(
				"Expect " + token_kind::name(delimiter) +
				" or " + token_kind::name(end) +
				", got " + token.info(get_buffer())
			);
		}
	}

	Lexer lexer;
};

} // namespace rin
