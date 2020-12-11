
#pragma once

#include "context.h"
#include "type.h"
#include "value.h"

namespace rin {

template<class R>
class ASTNode {
public:
	virtual R codegen(Context &ctx);
};

class ConstantNode : public ASTNode<std::unique_ptr<Value>> {
public:
	ConstantNode(std::unique_ptr<ASTNode<Type*>> type_node, const std::string &str):
		type_node(std::move(type_node)), str(str) {}

	std::unique_ptr<Value> codegen(Context &ctx) override;
private:
	std::unique_ptr<ASTNode<Type*>> type_node;
	std::string str;
};

} // namespace rin
