
#include <cassert>
#include <cctype>
#include <memory>
#include <numeric>
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

Ptr<FunctionTypeNode> Parser::take_function_type(
	size_t begin,
	Ptr<TypeNode> receiver_type
) {
	std::vector<TypeNode*> param_types;
	try {
		process_list(LPar, RPar, Comma, [&]() {
			param_types.push_back(take_type().release());
		});
		expect(lexer.take(), Arrow);
		auto result_type = take_type();
		return std::make_unique<FunctionTypeNode>(
			SourceRange(begin, lexer.position()),
			std::move(receiver_type), std::move(result_type),
			param_types
		);
	} catch (...) {
		for (auto type : param_types)
			delete type;
		throw;
	}
}

Ptr<TypeNode> Parser::take_type() {
	const auto begin = lexer.position();
	auto token = lexer.take();
	Ptr<TypeNode> ret;
	switch (token.kind) {
		case LPar: {
			if (lexer.peek().kind == RPar) {
				lexer.rewind_to(begin);
				ret = take_function_type(begin);
			} else {
				auto type = take_type();
				if (lexer.peek().kind == Comma) {
					lexer.rewind_to(begin);
					ret = take_function_type(begin);
				} else {
					expect(lexer.take(), RPar);
					if (lexer.peek().kind == Arrow) {
						lexer.rewind_to(begin);
						ret = take_function_type(begin);
					} else ret = std::move(type);
				}
			}
			break;
		}
		case Identifier:
			ret = std::make_unique<NamedTypeNode>(token, get_buffer());
			break;
		case Mul: case And: {
			bool const_flag = false;
			if (lexer.peek().kind == Const) {
				lexer.take();
				const_flag = true;
			}
			auto sub_type = take_type();
			SourceRange range(token.range.begin, lexer.position());
			if (token.kind == Mul)
				ret = std::make_unique<PointerTypeNode>(
					range,
					std::move(sub_type), const_flag
				);
			else
				ret = std::make_unique<RefTypeNode>(
					range,
					std::move(sub_type), const_flag
				);
			break;
		}
		case LBracket: {
			auto element_type = take_type();
			expect(lexer.take(), Comma);
			auto size = std::stoul(expect(lexer.take(), Number).content(get_buffer()));
			if (!size) throw ParseException("Array size must positive");
			expect(lexer.take(), RBracket);
			ret = std::make_unique<ArrayTypeNode>(
				SourceRange(token.range.begin, lexer.position()),
				std::move(element_type), size
			);
			break;
		}
		default: throw ParseException("Expected type, got " + token.info(get_buffer()));
	}
	if (lexer.peek().kind == Period) {
		auto cur = lexer.position();
		lexer.take();
		if (lexer.peek().kind == LPar)
			return take_function_type(begin, std::move(ret));
		else lexer.rewind_to(cur);
	}
	return ret;
}

Ptr<ValueNode> Parser::take_prim() {
	auto token = lexer.peek();
	switch (token.kind) {
		case Number:
		case True:
		case False:
			lexer.take();
			return std::make_unique<ConstantNode>(token, get_buffer());
		case LPar: {
			lexer.take();
			auto ret = take_expr();
			expect(lexer.take(), RPar);
			return ret;
		}
		case Identifier:
			lexer.take();
			return std::make_unique<NamedValueNode>(token, get_buffer());
		default: break;
	}
	return nullptr;
}

template<class T>
inline T pop_stack(std::stack<T> &st) {
	const T ret(st.top()); st.pop();
	return ret;
}

Ptr<ValueNode> Parser::take_expr() {
	enum {
		Empty, UnaryOp, BinOp, Prim
	} last = Empty;

	std::stack<ValueNode*> st;
	std::stack<Token> ops;
	auto error = [&st](const std::string &msg) {
		while (!st.empty()) {
			delete st.top();
			st.pop();
		}
		throw ParseException(msg);
	};
	auto require = [&](size_t amount) {
		if (st.size() < amount)
			error(
				"Not enough operand(s). "
				"Expected " + std::to_string(amount) +
				", got " + std::to_string(st.size())
			);
	};
	auto process_op = [&](std::stack<ValueNode*> &st, const Token &token) {
		const TokenKind op = token.kind;
		if (token_kind::is_unary_op(op)) {
			require(1);
			Ptr<ValueNode> value(pop_stack(st));
			st.push(new UnaryOpNode(std::move(value), token));
		} else {
			require(2);
			Ptr<ValueNode> rhs(pop_stack(st)), lhs(pop_stack(st));
			st.push(new BinOpNode(std::move(lhs), std::move(rhs), op));
		}
	};
	while (true) {
		auto token = lexer.peek();
		auto &kind = token.kind;
		if (kind == Eof) break;
		if (last != Prim)
			kind = token_kind::as_unary_op(kind);
		const bool is_unary = token_kind::is_unary_op(kind);
		const bool is_binary = token_kind::is_binary_op(kind);
		if (is_unary || is_binary) {
			if ((is_unary && (last == UnaryOp || last == Prim))
				|| (is_binary && (last != Prim)))
				error("Illegal operator: " + token_kind::name(kind));
			lexer.take();
			auto cur_pred = precedence_of(kind);
			while (!ops.empty() &&
				((precedence_of(ops.top().kind) < cur_pred) ||
				(precedence_of(ops.top().kind) == cur_pred && !is_right_associative(cur_pred)))
			) process_op(st, pop_stack(ops));
			ops.push(token);
			last = is_unary? UnaryOp: BinOp;
		} else if (auto ptr = take_prim()) {
			st.push(ptr.release());
			last = Prim;
		} else {
			// llvm::errs() << "Here: " << token.info(get_buffer()) << '\n' << ops.size() << '\n';
			break;
		}
	}
	while (!ops.empty()) process_op(st, pop_stack(ops));
	if (st.empty())
		error("Missing operand");
	assert(ops.empty() && st.size() == 1);
	return Ptr<ValueNode>(st.top());
}

Ptr<FunctionNode> Parser::take_function() {
	const auto begin = lexer.position();
	expect(lexer.take(), Fn);
	auto receiver_type = take_type();
	std::string name;
	if (lexer.peek().kind == Period) {
		lexer.take();
		name = expect(lexer.take(), Identifier).content(get_buffer());
	} else {
		if (auto ptr = ptr_cast<NamedTypeNode>(std::move(receiver_type)))
			name = ptr->get_name();
		else {
			throw ParseException(
				"Expected function name, got type " +
				receiver_type->to_string()
			);
		}
	}
	std::vector<TypeNode*> param_types;
	std::vector<std::string> param_names;
	Ptr<TypeNode> result_type;
	try {
		process_list(LPar, RPar, Comma, [&]() {
			param_names.push_back(
				expect(lexer.take(), Identifier)
					.content(get_buffer())
			);
			expect(lexer.take(), Colon);
			param_types.push_back(take_type().release());
		});
		if (lexer.peek().kind == Colon) {
			lexer.take();
			result_type = take_type();
		} else
			// TODO preserve some type names like this
			result_type =
				std::make_unique<NamedTypeNode>(
					SourceRange(lexer.position()),
					"void"
				);
	} catch (const std::exception &e) {
		for (auto type : param_types)
			delete type;
		throw;
	}
	auto func_type_node =
		std::make_unique<FunctionTypeNode>(
			SourceRange(begin, lexer.position()),
			std::move(receiver_type),
			std::move(result_type),
			param_types
		);
	auto prototype_node =
		std::make_unique<PrototypeNode>(
			func_type_node->get_source_range(),
			name,
			std::move(func_type_node),
			param_names
		);
	auto block = take_block();
	return std::make_unique<FunctionNode>(
		SourceRange(begin, lexer.position()),
		std::move(prototype_node),
		std::move(block)
	);
}

Ptr<BlockNode> Parser::take_block() {
	const auto begin = lexer.position();
	expect(lexer.take(), LBrace);
	std::vector<StmtNode*> stmts;
	try {
		while (lexer.peek().kind != RBrace)
			stmts.push_back(take_stmt().release());
		expect(lexer.take(), RBrace);
		return std::make_unique<BlockNode>(
			SourceRange(begin, lexer.position()),
			stmts
		);
	} catch (...) {
		for (auto stmt : stmts)
			delete stmt;
		throw;
	}
}

Ptr<StmtNode> Parser::take_stmt() {
	while (lexer.peek().kind == Semicolon) // empty statement
		lexer.take();
	const auto begin = lexer.position();
	switch (lexer.peek().kind) {
		case Let: {
			lexer.take();
			bool const_flag = true;
			if (lexer.peek().kind == Mul) {
				lexer.take();
				const_flag = false;
			}
			auto name = expect(lexer.take(), Identifier).content(get_buffer());
			Ptr<TypeNode> type_node;
			Ptr<ValueNode> value_node;
			if (lexer.peek().kind == Colon) {
				lexer.take();
				type_node = take_type();
			}
			if (lexer.peek().kind == Assign) {
				lexer.take();
				value_node = take_expr();
			}
			expect(lexer.take(), Semicolon);
			return std::make_unique<VarDeclNode>(
				SourceRange(begin, lexer.position()),
				name, std::move(type_node), std::move(value_node),
				const_flag
			);
		}
		case LBrace: return take_block();
		case Return: {
			lexer.take();
			if (lexer.peek().kind == Semicolon) {
				lexer.take();
				return std::make_unique<ReturnNode>(
					SourceRange(begin, lexer.position()),
					nullptr
				);
			} else {
				auto type = take_expr();
				expect(lexer.take(), Semicolon);
				return std::make_unique<ReturnNode>(
					SourceRange(begin, lexer.position()),
					std::move(type)
				);
			}
		}
		default: {
			auto ret = take_expr();
			expect(lexer.take(), Semicolon);
			return ret;
		}
	}
}

} // namespace rin
