
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
	Ptr<ASTNode> take_stmt();
	Ptr<BlockNode> take_block();
	Ptr<FunctionNode> take_function();
	Ptr<DeclNode> take_decl();
	Ptr<TopLevelNode> take_top_level();
private:
	Ptr<ASTNode> take_prim_inner();
	std::string take_name() {
		return std::string(expect(lexer.take(), TokenKind::Identifier)
				.content(get_buffer()));
	}

	template<class...Args>
	[[noreturn]] void error(const char *pattern, Args &&...args) const {
		throw ParseException(fmt::format(pattern, std::forward<Args>(args)...));
	}

	void expect_end_of_stmt() {
		// TODO error message
		expect(lexer.take(), TokenKind::Semicolon);
	}
	Token expect(const Token &&token, TokenKind kind) const {
		if (token.kind != kind) // TODO unlikely
			error(
				"Expected {}, got {}",
				token_kind::name(kind),
				token.info(get_buffer())
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
			error(
				"Expected {} or {}, got {}",
				token_kind::name(delimiter),
				token_kind::name(end),
				token.info(get_buffer())
			);
		}
	}

	Lexer lexer;
};

} // namespace rin
