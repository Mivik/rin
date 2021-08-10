
#pragma once

#include "codegen.h"
#include "lexer.h"
#include "token.h"
#include "value.h"

namespace rin {

class ASTNode {
protected:
	explicit ASTNode(const SourceRange &range): range(range) {}

	SourceRange range;
public:
	virtual ~ASTNode() = default;

	[[nodiscard]] SourceRange get_source_range() const { return range; }

	virtual Value codegen(Codegen &ctx) const = 0;
};

#define OVERRIDE \
    Value codegen(Codegen &ctx) const override;

class ConstantNode final : public ASTNode {
public:
	ConstantNode(const Token &token, const Reader &input):
		ASTNode(token.range), content(input.substr(token.range)) {}

	[[nodiscard]] std::string_view get_content() const { return content; }

	OVERRIDE
private:
	std::string_view content;
};

class UnaryOpNode final : public ASTNode {
public:
	UnaryOpNode(Ptr<ASTNode> value, const Token &token):
		ASTNode(token.range + value->get_source_range()),
		value_node(std::move(value)), op(token.kind) {
		assert(token_kind::is_unary_op(op));
	}

	[[nodiscard]] const ASTNode *get_value_node() const { return value_node.get(); }

	OVERRIDE
private:
	Ptr<ASTNode> value_node;
	TokenKind op;
};

class BinOpNode final : public ASTNode {
public:
	BinOpNode(Ptr<ASTNode> lhs, Ptr<ASTNode> rhs, TokenKind op):
		ASTNode(lhs->get_source_range() + rhs->get_source_range()),
		lhs_node(std::move(lhs)), rhs_node(std::move(rhs)), op(op) {
		assert(token_kind::is_binary_op(op));
	}

	[[nodiscard]] const ASTNode *get_lhs_node() const { return lhs_node.get(); }
	[[nodiscard]] const ASTNode *get_rhs_node() const { return rhs_node.get(); }

	OVERRIDE
private:
	Ptr<ASTNode> lhs_node, rhs_node;
	TokenKind op;
};

class ValueNode final : public ASTNode {
public:
	ValueNode(const Token &token, const Reader &input):
		ASTNode(token.range), name(input.substr(token.range)) {}

	[[nodiscard]] std::string_view get_name() const { return name; }

	OVERRIDE
private:
	std::string_view name;
};

class CallNode final : public ASTNode {
public:
	CallNode(
		const SourceRange &range,
		std::string name,
		Ptr<ASTNode> receiver_node,
		std::vector<Ptr<ASTNode>> argument_nodes
	): ASTNode(range),
	   name(std::move(name)),
	   receiver_node(std::move(receiver_node)),
	   argument_nodes(std::move(argument_nodes)) {}

	[[nodiscard]] std::string get_function_name() const { return name; }
	[[nodiscard]] const ASTNode *get_receiver_node() const { return receiver_node.get(); }
	[[nodiscard]] const std::vector<Ptr<ASTNode>> &get_argument_nodes() const { return argument_nodes; }

	OVERRIDE
private:
	std::string name;
	Ptr<ASTNode> receiver_node;
	std::vector<Ptr<ASTNode>> argument_nodes;
};

#undef OVERRIDE

} // namespace rin
