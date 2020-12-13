
#include <cassert>
#include <numeric>
#include <cctype>
#include <memory>
#include <stack>

#include "parser.h"

namespace rin {

inline int precedence_of(TokenKind op) {
	switch (op) {
		case UAdd: case USub:
		case Not: case LNot:
			return 2;
		case Mul:
		case Div:
		case Mod:
			return 3;
		case Add:
		case Sub:
			return 4;
		case Shl: case Shr:
			return 5;
		case Lt: case Le:
		case Gt: case Ge:
			return 7;
		case Eq: case Neq:
			return 8;
		case And: return 9;
		case Xor: return 10;
		case Or: return 11;
		case LAnd: return 12;
		case LOr: return 13;
		case Assign:
		case AddA: case SubA:
		case MulA: case DivA: case ModA:
		case ShlA: case ShrA:
		case AndA: case OrA: case XorA:
			return 14;
		default: return std::numeric_limits<int>::max();
	}
}

// It's guaranteed that operators of the same precedence have the same associativity.
inline bool is_right_associative(int precedence) {
	switch (precedence) {
		case 2:
		case 14:
			return true;
		default: return false;
	}
}

Ptr<ASTNode<Value>> Parser::take_prim() {
	auto token = lexer.peek();
	switch (token.kind) {
		case Number:
		case True:
		case False:
			lexer.take();
			return std::make_unique<ConstantNode>(token.content(get_buffer()));
		case LPar: {
			lexer.take();
			auto ret = take_expr();
			expect(lexer.take(), RPar);
			return ret;
		}
		default: break;
	}
	return nullptr;
}

template<class T>
inline T pop_stack(std::stack<T> &st) {
	const T ret(st.top()); st.pop();
	return ret;
}

inline void process_op(std::stack<ASTNode<Value>*> &st, TokenKind op) {
	if (token_kind::is_unary_op(op)) {
		Ptr<ASTNode<Value>> value(pop_stack(st));
		st.push(new UnaryOpNode(std::move(value), op));
	} else {
		Ptr<ASTNode<Value>> rhs(pop_stack(st)), lhs(pop_stack(st));
		st.push(new BinOpNode(std::move(lhs), std::move(rhs), op));
	}
}

Ptr<ASTNode<Value>> Parser::take_expr() {
	std::stack<ASTNode<Value>*> st;
	std::stack<TokenKind> ops;
	bool expecting_binop = false;
	while (true) {
		auto token = lexer.peek();
		if (!token) break;
		if (!expecting_binop)
			token.kind = token_kind::as_unary_op(token.kind);
		if (token_kind::is_unary_op(token.kind) || token_kind::is_binary_op(token.kind)) {
			lexer.take();
			auto cur_pred = precedence_of(token.kind);
			while (!ops.empty() &&
				((precedence_of(ops.top()) < cur_pred) ||
				(precedence_of(ops.top()) == cur_pred && !is_right_associative(cur_pred)))
			) process_op(st, pop_stack(ops));
			ops.push(token.kind);
			expecting_binop = false;
		} else if (auto ptr = take_prim()) {
			st.push(ptr.release());
			expecting_binop = true;
		} else break;
	}
	while (!ops.empty()) process_op(st, pop_stack(ops));
	assert(ops.empty() && st.size() == 1);
	return Ptr<ASTNode<Value>>(st.top());
}

} // namespace rin
