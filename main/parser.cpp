
#include <stack>

#include "parser.h"

namespace rin {

using K = TokenKind;

Ptr<ASTNode> Parser::take_prim() {
	const auto begin = lexer.position();
	auto token = lexer.peek();
	switch (token.kind) {
		case K::Number:
		case K::True:
		case K::False: {
			lexer.take();
			return std::make_unique<ConstantNode>(token, get_reader());
		}
		case K::LPar: {
			lexer.take();
			auto ret = take_expr();
			expect(lexer.take(), K::RPar);
			return ret;
		}
		case K::Identifier: {
			lexer.take();
			if (lexer.peek().kind == K::LPar) {
				std::vector<Ptr<ASTNode>> args;
				process_list(K::LPar, K::RPar, K::Comma, [&]() {
					args.push_back(std::move(take_expr()));
				});
				return std::make_unique<CallNode>(
					SourceRange(begin, lexer.position()),
					std::string(token.content(get_buffer())),
					nullptr,
					std::move(args)
				);
			}
			return std::make_unique<ValueNode>(token, get_reader());
		}
		default:
			throw ParseException("Unsupported token type: " + token_kind::name(token.kind));
	}
}

template<class T>
inline T pop_stack(std::stack<T> &st) {
	T ret(std::move(st.top()));
	st.pop();
	return ret;
}

// It's guaranteed that operators of the same precedence have the same associativity.
inline bool is_right_associative(int precedence) {
	switch (precedence) {
		case 2:
		case 14:
			return true;
		default:
			return false;
	}
}

Ptr<ASTNode> Parser::take_expr() {
	enum {
		Empty, UnaryOp, BinOp, Prim
	} last = Empty;

	using token_kind::precedence_of;

	std::stack<Ptr<ASTNode>> st;
	std::stack<Token> ops;
	auto require = [&](size_t amount) {
		if (st.size() < amount)
			throw ParseException(
				"Not enough operand(s). "
				"Expected " + std::to_string(amount) +
				", got " + std::to_string(st.size())
			);
	};
	auto process_op = [&](std::stack<Ptr<ASTNode>> &st, const Token &token) {
		const TokenKind op = token.kind;
		if (token_kind::is_unary_op(op)) {
			require(1);
			Ptr<ASTNode> value(pop_stack(st));
			st.push(std::make_unique<UnaryOpNode>(std::move(value), token));
		} else {
			require(2);
			Ptr<ASTNode> rhs(pop_stack(st)), lhs(pop_stack(st));
			st.push(std::make_unique<BinOpNode>(std::move(lhs), std::move(rhs), op));
		}
	};
	while (true) {
		auto token = lexer.peek();
		auto &kind = token.kind;
		if (kind == TokenKind::Eof) break;
		if (last != Prim)
			kind = token_kind::as_unary_op(kind);
		if (kind == TokenKind::LBracket) {
			lexer.take();
			auto index = take_expr();
			expect(lexer.take(), TokenKind::RBracket);
			auto cur_pred = 2;
			while (!ops.empty() &&
				   ((precedence_of(ops.top().kind) < cur_pred) ||
					(precedence_of(ops.top().kind) == cur_pred && !is_right_associative(cur_pred)))
				)
				process_op(st, pop_stack(ops));
			ops.push(token);
			st.push(std::move(index));
			last = Prim;
			continue;
		}
		const bool is_unary = token_kind::is_unary_op(kind);
		const bool is_binary = token_kind::is_binary_op(kind);
		if (is_unary || is_binary) {
			if ((is_unary && (last == UnaryOp || last == Prim))
				|| (is_binary && (last != Prim)))
				throw ParseException("Illegal operator: " + token_kind::name(kind));
			lexer.take();
			auto cur_pred = precedence_of(kind);
			while (!ops.empty() &&
				   ((precedence_of(ops.top().kind) < cur_pred) ||
					(precedence_of(ops.top().kind) == cur_pred && !is_right_associative(cur_pred)))
				)
				process_op(st, pop_stack(ops));
			ops.push(token);
			last = is_unary? UnaryOp: BinOp;
		} else if (last != Prim) {
			if (auto ptr = take_prim()) {
				st.push(std::move(ptr));
				last = Prim;
			} else break;
		} else break;
	}
	while (!ops.empty()) process_op(st, pop_stack(ops));
	if (st.empty())
		throw ParseException("Missing operand");
	assert(ops.empty() && st.size() == 1);
	return std::move(st.top());
}

} // namespace rin
