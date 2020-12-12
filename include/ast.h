
#pragma once

#include "context.h"
#include "type.h"
#include "value.h"
#include "utility.h"

namespace rin {

template<class R>
class ASTNode {
public:
	virtual R codegen(Context &ctx);
};

class ConstantNode : public ASTNode<Ptr<Value>> {
public:
	ConstantNode(Ptr<ASTNode<Type*>> type_node, const std::string &str):
		type_node(std::move(type_node)), str(str) {}

	Ptr<Value> codegen(Context &ctx) override;
private:
	Ptr<ASTNode<Type*>> type_node;
	std::string str;
};

} // namespace rin
