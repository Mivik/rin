
#pragma once

#include "context.h"
#include "type.h"
#include "value.h"
#include "token.h"

namespace rin {

template<class R>
class ASTNode {
public:
	virtual R codegen(Context &ctx) = 0;

	virtual ~ASTNode() = default;
};

class ConstantNode : public ASTNode<Value> {
public:
	ConstantNode(const std::string &str): str(str) {}

	Value codegen(Context &ctx) override;
private:
	std::string str;
};

class BinOpNode : public ASTNode<Value> {
public:
	BinOpNode(Ptr<ASTNode<Value>> lhs, Ptr<ASTNode<Value>> rhs, TokenKind op):
		lhs_node(std::move(lhs)), rhs_node(std::move(rhs)), op(op) {}

	Value codegen(Context &ctx) override;
private:
	Ptr<ASTNode<Value>> lhs_node, rhs_node;
	TokenKind op;
};

} // namespace rin
