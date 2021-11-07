
#include <stack>

#include "parser.h"

namespace rin {

using K = TokenKind;

Ptr<ASTNode> Parser::take_prim_inner() {
	auto token = lexer.peek();
	const auto begin = lexer.position();
	switch (token.kind) {
		case K::Number:
		case K::True:
		case K::False: {
			lexer.take();
			return std::make_unique<ConstantNode>(token, get_reader());
		}
		case K::LBracket: {
			lexer.take();
			auto type = take_prim();
			expect(lexer.take(), K::Semicolon);
			auto length = take_prim();
			expect(lexer.take(), K::RBracket);
			return std::make_unique<ArrayTypeNode>(
				SourceRange(begin, lexer.position()),
				std::move(type),
				std::move(length)
			);
		}
		case K::LBrace: {
			std::vector<Ptr<ASTNode>> elements;
			process_list(K::LBrace, K::RBrace, K::Comma, [&]() {
				elements.push_back(take_expr());
			});
			return std::make_unique<TupleValueNode>(
				SourceRange(begin, lexer.position()),
				std::move(elements)
			);
		}
		case K::LPar: {
			lexer.take();
			auto ret = take_expr();
			if (lexer.peek().kind == K::Comma) {
				std::vector<Ptr<ASTNode>> elements;
				elements.push_back(std::move(ret));
				process_list(K::Comma, K::RPar, K::Comma, [&]() {
					elements.push_back(take_expr());
				});
				return std::make_unique<TupleNode>(
					SourceRange(begin, lexer.position()),
					std::move(elements)
				);
			}
			expect(lexer.take(), K::RPar);
			return ret;
		}
		case K::Builtin:
		case K::Identifier: {
			lexer.take();
			// TODO function object
			if (lexer.peek().kind == K::LPar) {
				std::vector<Ptr<ASTNode>> args;
				process_list(K::LPar, K::RPar, K::Comma, [&]() {
					args.push_back(take_expr());
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
		case K::If: {
			lexer.take();
			expect(lexer.take(), K::LPar);
			auto cond = take_expr();
			expect(lexer.take(), K::RPar);
			auto body = take_stmt();
			Ptr<ASTNode> else_body;
			if (lexer.peek().kind == K::Else) {
				lexer.take();
				else_body = take_stmt();
			}
			return std::make_unique<IfNode>(
				SourceRange(begin, lexer.position()),
				std::move(cond),
				std::move(body),
				std::move(else_body)
			);
		}
		case K::Struct: {
			lexer.take();
			std::vector<std::string> field_names;
			std::vector<Ptr<ASTNode>> field_types;
			process_list(K::LBrace, K::RBrace, K::Comma, [&]() {
				field_names.emplace_back(take_name());
				expect(lexer.take(), K::Colon);
				field_types.push_back(take_expr());
			});
			return std::make_unique<StructNode>(
				SourceRange(begin, lexer.position()),
				std::move(field_names),
				std::move(field_types)
			);
		}

		default: {
			token.kind = token_kind::as_unary_op(token.kind);
			if (token_kind::is_unary_op(token.kind)) {
				lexer.take();
				if (lexer.peek().kind == K::Mut) {
					if (token.kind == K::Pointer) {
						lexer.take();
						token.kind = K::PointerMut;
					} else if (token.kind == K::Ref) {
						lexer.take();
						token.kind = K::RefMut;
					}
				}
				return std::make_unique<UnaryOpNode>(take_prim(), token);
			}
			return nullptr;
		}
	}
}

Ptr<ASTNode> Parser::take_prim() {
	const auto start = lexer.position();
	auto node = take_prim_inner();
	if (lexer.peek().kind == K::LBrace) {
		std::vector<Ptr<ASTNode>> fields;
		process_list(K::LBrace, K::RBrace, K::Comma, [&]() {
			fields.push_back(take_expr());
		});
		return std::make_unique<StructValueNode>(
			SourceRange(start, lexer.position()),
			std::move(node),
			std::move(fields)
		);
	}
	return node;
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
			error(
				"Not enough operand(s). Expected {}, got {}",
				amount, st.size()
			);
	};
	auto process_op = [&](std::stack<Ptr<ASTNode>> &st, Token
	token) {
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
		if (kind == K::Eof) break;
		if (last != Prim)
			kind = token_kind::as_unary_op(kind);
		if (kind == K::LBracket) {
			lexer.take();
			auto index = take_expr();
			expect(lexer.take(), K::RBracket);
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
				error("Illegal operator: {}", token_kind::name(kind));
			lexer.take();
			if (lexer.peek().kind == K::Mut) {
				if (kind == K::Pointer) {
					lexer.take();
					kind = K::PointerMut;
				} else if (kind == K::Ref) {
					lexer.take();
					kind = K::RefMut;
				}
			}
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
		error("Missing operand");
	assert(ops.empty() && st.size() == 1);
	return std::move(st.top());
}

Ptr<BlockNode> Parser::take_block() {
	// TODO trim
	const auto begin = lexer.position();
	expect(lexer.take(), K::LBrace);
	std::vector<Ptr<ASTNode>> stmts;
	while (lexer.peek().kind != K::RBrace)
		stmts.push_back(take_stmt());
	lexer.take();
	return std::make_unique<BlockNode>(
		SourceRange(begin, lexer.position()),
		std::move(stmts)
	);
}

Ptr<FunctionNode> Parser::take_function(bool is_inline) {
	const auto begin = lexer.position();
	std::vector<std::pair<std::string, Ptr<ASTNode>>> template_parameters;
	if (lexer.peek().kind == K::Lt)
		process_list(K::Lt, K::Gt, K::Comma, [&]() {
			auto name = take_name();
			Ptr<ASTNode> concept_node;
			if (lexer.peek().kind == K::Colon) {
				lexer.take();
				concept_node = take_prim();
			}
			template_parameters.emplace_back(name, std::move(concept_node));
		});
	expect(lexer.take(), K::Fn);
	Ptr<ASTNode> receiver_type;
	if (lexer.peek().kind == K::LBracket) {
		lexer.take();
		receiver_type = take_expr();
		expect(lexer.take(), K::RBracket);
		expect(lexer.take(), K::Period);
	}
	auto name = take_name();

	std::vector<Ptr<ASTNode>> param_types;
	std::vector<std::string> param_names;
	Ptr<ASTNode> result_type;
	process_list(K::LPar, K::RPar, K::Comma, [&]() {
		param_names.emplace_back(take_name());
		expect(lexer.take(), K::Colon);
		param_types.push_back(take_expr());
	});
	if (lexer.peek().kind == K::Colon) {
		lexer.take();
		result_type = take_prim_inner();
	} else
		// TODO preserve some type names like this
		result_type =
			std::make_unique<ValueNode>(
				SourceRange(lexer.position()),
				"void"
			);
	auto type_node =
		std::make_unique<FunctionTypeNode>(
			SourceRange(begin, lexer.position()),
			std::move(template_parameters),
			std::move(receiver_type),
			std::move(result_type),
			std::move(param_types),
			std::move(param_names)
		);
	Ptr<ASTNode> content;
	if (lexer.peek().kind == K::Assign) {
		lexer.take();
		content = take_expr();
		expect_end_of_stmt();
	} else if (lexer.peek().kind == K::LBrace) content = take_block();
	else expect(lexer.take(), K::Semicolon);
	return std::make_unique<FunctionNode>(
		SourceRange(begin, lexer.position()),
		name,
		std::move(type_node),
		std::move(content),
		is_inline
	);
}

Ptr<ASTNode> Parser::take_stmt() {
	while (lexer.peek().kind == K::Semicolon) lexer.take();
	const auto begin = lexer.position();
	switch (auto kind = lexer.peek().kind) {
		case K::LBrace:
			return take_block();
		case K::Var:
		case K::Val:
		case K::Inline: {
			lexer.take();
			if (lexer.peek().kind == K::Fn) return take_function(true);
			bool is_inline = kind == K::Inline;
			if (is_inline) {
				auto token = lexer.take();
				kind = token.kind;
				if (kind != K::Var && kind != K::Val)
					error("Unexpected {} after inline", token.info(get_buffer()));
			}
			auto name = take_name();
			Ptr<ASTNode> type_node, value_node;
			if (lexer.peek().kind == K::Colon) {
				lexer.take();
				type_node = take_prim();
			}
			if (lexer.peek().kind == K::Assign) {
				lexer.take();
				value_node = take_expr();
			}
			expect_end_of_stmt();
			auto t = std::make_unique<VarDeclNode>(
				SourceRange(begin, lexer.position()),
				std::string(name),
				std::move(type_node),
				std::move(value_node),
				kind == K::Var,
				is_inline
			);
			return t;
		}
		case K::Return: {
			lexer.take();
			if (lexer.peek().kind == K::Semicolon) {
				lexer.take();
				return std::make_unique<ReturnNode>(
					SourceRange(begin, lexer.position()),
					nullptr
				);
			} else {
				auto expr = take_expr();
				expect_end_of_stmt();
				return std::make_unique<ReturnNode>(
					SourceRange(begin, lexer.position()),
					std::move(expr)
				);
			}
		}
		case K::If: {
			lexer.take();
			expect(lexer.take(), K::LPar);
			auto cond = take_expr();
			expect(lexer.take(), K::RPar);
			auto body = take_stmt();
			Ptr<ASTNode> else_body;
			if (lexer.peek().kind == K::Else) {
				lexer.take();
				else_body = take_stmt();
			}
			return std::make_unique<IfNode>(
				SourceRange(begin, lexer.position()),
				std::move(cond),
				std::move(body),
				std::move(else_body)
			);
		}
		default: {
			auto res = take_expr();
			expect_end_of_stmt();
			return res;
		}
	}
}

Ptr<DeclNode> Parser::take_decl() {
	const auto begin = lexer.position();
	switch (auto kind = lexer.peek().kind) {
		case K::Var:
		case K::Val:
		case K::Inline: {
			lexer.take();
			if (lexer.peek().kind == K::Fn) return take_function(true);
			bool is_inline = kind == K::Inline;
			if (is_inline) {
				auto token = lexer.take();
				kind = token.kind;
				if (kind != K::Var && kind != K::Val)
					error("Unexpected {} after inline", token.info(get_buffer()));
			}
			auto name = take_name();
			Ptr<ASTNode> type_node, value_node;
			if (lexer.peek().kind == K::Colon) {
				lexer.take();
				type_node = take_prim();
			}
			if (lexer.peek().kind == K::Assign) {
				lexer.take();
				value_node = take_expr();
			}
			expect_end_of_stmt();
			return std::make_unique<GlobalVarDeclNode>(
				SourceRange(begin, lexer.position()),
				std::string(name),
				std::move(type_node),
				std::move(value_node),
				kind == K::Var,
				is_inline
			);
		}
		case K::Fn:
		case K::Lt:
			return take_function();
		default:
			error("Unexpected token: {}", lexer.peek().info(get_buffer()));
	}
}

Ptr<TopLevelNode> Parser::take_top_level() {
	const auto begin = lexer.position();
	std::vector<Ptr<DeclNode>>
		children;
	while (lexer.peek().kind != K::Eof)
		children.push_back(take_decl());
	return std::make_unique<TopLevelNode>(
		SourceRange(begin, lexer.position()),
		std::move(children)
	);
}

} // namespace rin
