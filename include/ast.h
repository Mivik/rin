
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

class ConstantNode : public ASTNode {
public:
	ConstantNode(const Token &token, const Reader &input):
		ASTNode(token.range), content(input.substr(token.range)) {}

	[[nodiscard]] std::string_view get_content() const { return content; }

	OVERRIDE
private:
	std::string_view content;
};

class UnaryOpNode : public ASTNode {
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

class BinOpNode : public ASTNode {
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

class ValueNode : public ASTNode {
public:
	ValueNode(const Token &token, const Reader &input):
		ASTNode(token.range), name(input.substr(token.range)) {}

	[[nodiscard]] std::string_view get_name() const { return name; }

	OVERRIDE
private:
	std::string_view name;
};

#undef OVERRIDE

} // namespace rin
