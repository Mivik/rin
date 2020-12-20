
#pragma once

#include "ast.h"
#include "lexer.h"

namespace rin {

class ParseException : public std::exception {
public:
	const char *what() const noexcept { return msg.data(); }

	ParseException(const std::string &msg): msg(msg) {}
private:
	std::string msg;
};

class Parser {
public:
	Parser(const char *str): Parser(MemoryBuffer(str)) {}
	Parser(const MemoryBuffer &buffer): lexer(buffer) {}

	Ptr<TypeNode> take_type();
	Ptr<ValueNode> take_prim();
	Ptr<ValueNode> take_expr();
	Ptr<StmtNode> take_stmt();
	Ptr<BlockNode> take_block();
	Ptr<FunctionNode> take_function();
	inline const MemoryBuffer& get_buffer() const { return lexer.get_buffer(); }
private:
	Ptr<FunctionTypeNode> take_function_type(
		size_t begin,
		Ptr<TypeNode> receiver_type = nullptr
	);

	inline const Token&& expect(const Token &&token, TokenKind kind) {
		if (token.kind != kind)
			throw ParseException(
				"Expected " + token_kind::name(kind) + ", "
				+ "got " + token.info(get_buffer()));
		return std::move(token);
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
