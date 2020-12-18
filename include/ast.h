
#pragma once

#include <string>

#include "context.h"
#include "type.h"
#include "value.h"
#include "token.h"

namespace rin {

template<class R>
class ASTNode {
protected:
	ASTNode(const SourceRange &range): range(range) {}

	SourceRange range;
public:
	inline SourceRange get_source_range() const { return range; }

	virtual R codegen(Context &ctx) const = 0;
	virtual std::string to_string() const = 0;

	virtual ~ASTNode() = default;
};

class ConstantNode : public ASTNode<Value> {
public:
	ConstantNode(const Token &token, const MemoryBuffer &buffer):
		ASTNode(token.range), str(token.content(buffer)) {}

	inline std::string get_string_value() const { return str; }

	Value codegen(Context &ctx) const override;
	std::string to_string() const override;
private:
	std::string str;
};

class UnaryOpNode : public ASTNode<Value> {
public:
	UnaryOpNode(Ptr<ASTNode<Value>> value, const Token &token):
		ASTNode(token.range + value->get_source_range()),
		value_node(std::move(value)), op(token.kind) {
		assert(token_kind::is_unary_op(op));
	}

	inline const ASTNode<Value>* get_value_node() const { return value_node.get(); }

	Value codegen(Context &ctx) const override;
	std::string to_string() const override;
private:
	Ptr<ASTNode<Value>> value_node;
	TokenKind op;
};

class BinOpNode : public ASTNode<Value> {
public:
	BinOpNode(Ptr<ASTNode<Value>> lhs, Ptr<ASTNode<Value>> rhs, TokenKind op):
		ASTNode(lhs->get_source_range() + rhs->get_source_range()),
		lhs_node(std::move(lhs)), rhs_node(std::move(rhs)), op(op) {
		assert(token_kind::is_binary_op(op));
	}

	inline const ASTNode<Value>* get_lhs_node() const { return lhs_node.get(); }
	inline const ASTNode<Value>* get_rhs_node() const { return rhs_node.get(); }

	Value codegen(Context &ctx) const override;
	std::string to_string() const override;
private:
	Ptr<ASTNode<Value>> lhs_node, rhs_node;
	TokenKind op;
};

class ValueNode : public ASTNode<Value> {
public:
	ValueNode(const Token &token, const MemoryBuffer &buffer):
		ASTNode(token.range), name(token.content(buffer)) {}

	inline std::string get_name() const { return name; }
	Value codegen(Context &ctx) const override;
	std::string to_string() const override;
private:
	std::string name;
};

} // namespace rin
