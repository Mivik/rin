
#pragma once

#include <string>
#include <utility>

#include "codegen.h"
#include "context.h"
#include "prototype.h"
#include "token.h"
#include "type.h"
#include "value.h"

namespace rin {

template<class R>
class ASTNode {
protected:
	explicit ASTNode(const SourceRange &range): range(range) {}

	SourceRange range;
public:
	[[nodiscard]] inline SourceRange get_source_range() const { return range; }

	virtual R codegen(Context &ctx) const = 0;
	[[nodiscard]] virtual std::string to_string() const = 0;

	virtual ~ASTNode() = default;
};

class ValueNode : public ASTNode<Value> {
protected:
	explicit ValueNode(const SourceRange &range): ASTNode(range) {}
public:
	[[nodiscard]] bool has_return() const;
};

using StmtNode = ValueNode;
using TypeNode = ASTNode<Type *>;
using DeclNode = ASTNode<void>;

#define OVERRIDE(R) \
    R codegen(Context &ctx) const override; \
    std::string to_string() const override;

class ConstantNode : public ValueNode {
public:
	ConstantNode(const Token &token, const MemoryBuffer &buffer):
		ValueNode(token.range), str(token.content(buffer)) {}

	[[nodiscard]] inline const std::string &get_string_value() const { return str; }

	OVERRIDE(Value)
private:
	std::string str;
};

class UnaryOpNode : public ValueNode {
public:
	UnaryOpNode(Ptr<ValueNode> value, const Token &token):
		ValueNode(token.range + value->get_source_range()),
		value_node(std::move(value)), op(token.kind) {
		assert(token_kind::is_unary_op(op));
	}

	[[nodiscard]] inline const ValueNode *get_value_node() const { return value_node.get(); }

	OVERRIDE(Value)
private:
	Ptr<ValueNode> value_node;
	TokenKind op;
};

class BinOpNode : public ValueNode {
public:
	BinOpNode(Ptr<ValueNode> lhs, Ptr<ValueNode> rhs, TokenKind op):
		ValueNode(lhs->get_source_range() + rhs->get_source_range()),
		lhs_node(std::move(lhs)), rhs_node(std::move(rhs)), op(op) {
		assert(token_kind::is_binary_op(op));
	}

	[[nodiscard]] inline const ValueNode *get_lhs_node() const { return lhs_node.get(); }
	[[nodiscard]] inline const ValueNode *get_rhs_node() const { return rhs_node.get(); }

	OVERRIDE(Value)
private:
	Ptr<ValueNode> lhs_node, rhs_node;
	TokenKind op;
};

class NamedTypeNode : public TypeNode {
public:
	NamedTypeNode(const SourceRange &range, std::string name):
		ASTNode(range), name(std::move(name)) {}
	NamedTypeNode(const Token &token, const MemoryBuffer &buffer):
		ASTNode(token.range), name(token.content(buffer)) {}

	[[nodiscard]] inline const std::string &get_name() const { return name; }

	OVERRIDE(Type*)
private:
	std::string name;
};

class ArrayTypeNode : public TypeNode {
public:
	ArrayTypeNode(
		const SourceRange &range,
		Ptr<TypeNode> element_type_node,
		unsigned int size
	): ASTNode(range),
	   element_type_node(std::move(element_type_node)),
	   size(size) {}

	[[nodiscard]] inline const TypeNode *get_element_type_node() const { return element_type_node.get(); }
	[[nodiscard]] inline unsigned int get_size() const { return size; }

	OVERRIDE(Type*)
private:
	Ptr<TypeNode> element_type_node;
	unsigned int size;
};

class PointerTypeNode : public TypeNode {
public:
	PointerTypeNode(
		const SourceRange &range,
		Ptr<TypeNode> sub_type_node,
		bool const_flag
	): ASTNode(range),
	   sub_type_node(std::move(sub_type_node)),
	   const_flag(const_flag) {}

	[[nodiscard]] inline const TypeNode *get_sub_type_node() const { return sub_type_node.get(); }
	[[nodiscard]] inline bool is_const() const { return const_flag; }

	OVERRIDE(Type*)
private:
	Ptr<TypeNode> sub_type_node;
	bool const_flag;
};

class RefTypeNode : public TypeNode {
public:
	RefTypeNode(
		const SourceRange &range,
		Ptr<TypeNode> sub_type_node,
		bool const_flag
	): ASTNode(range),
	   sub_type_node(std::move(sub_type_node)),
	   const_flag(const_flag) {}

	[[nodiscard]] inline const TypeNode *get_sub_type_node() const { return sub_type_node.get(); }
	[[nodiscard]] inline bool is_const() const { return const_flag; }

	OVERRIDE(Type*)
private:
	Ptr<TypeNode> sub_type_node;
	bool const_flag;
};

class FunctionTypeNode : public TypeNode {
public:
	FunctionTypeNode(
		const SourceRange &range,
		Ptr<TypeNode> receiver_type_node,
		Ptr<TypeNode> result_type_node,
		std::vector<TypeNode *> param_type_nodes
	): ASTNode(range),
	   receiver_type_node(std::move(receiver_type_node)),
	   result_type_node(std::move(result_type_node)),
	   param_type_nodes(std::move(param_type_nodes)) {}

	[[nodiscard]] inline const TypeNode *get_receiver_type_node() const {
		return receiver_type_node.get();
	}
	[[nodiscard]] inline const TypeNode *get_result_type_node() const {
		return result_type_node.get();
	}
	[[nodiscard]] inline const std::vector<TypeNode *> &get_parameter_type_nodes() const {
		return param_type_nodes;
	}

	~FunctionTypeNode() override;

	OVERRIDE(Type*)
private:
	Ptr<TypeNode> receiver_type_node, result_type_node;
	std::vector<TypeNode *> param_type_nodes;
};

class PrototypeNode : public ASTNode<Prototype> {
public:
	PrototypeNode(
		const SourceRange &range,
		std::string name,
		Ptr<FunctionTypeNode> type_node,
		const std::vector<std::string> &param_names
	): ASTNode(range), name(std::move(name)),
	   type_node(std::move(type_node)),
	   param_names(param_names) {
		assert(param_names.size() ==
			   this->type_node->get_parameter_type_nodes().size());
	}

	[[nodiscard]] inline const std::string &get_name() const { return name; }
	[[nodiscard]] inline const FunctionTypeNode *get_type_node() const {
		return type_node.get();
	}
	[[nodiscard]] inline const std::vector<std::string> &get_parameter_names() const {
		return param_names;
	}

	OVERRIDE(Prototype)
private:
	std::string name;
	Ptr<FunctionTypeNode> type_node;
	std::vector<std::string> param_names;
};

class NamedValueNode : public ValueNode {
public:
	NamedValueNode(const Token &token, const MemoryBuffer &buffer):
		ValueNode(token.range), name(token.content(buffer)) {}

	[[nodiscard]] inline const std::string &get_name() const { return name; }

	OVERRIDE(Value)
private:
	std::string name;
};

class VarDeclNode : public StmtNode {
public:
	VarDeclNode(
		const SourceRange &range,
		std::string name,
		Ptr<TypeNode> type_node,
		Ptr<ValueNode> value_node,
		bool const_flag
	): StmtNode(range), name(std::move(name)),
	   type_node(std::move(type_node)), value_node(std::move(value_node)),
	   const_flag(const_flag) {
		if (!(this->type_node || this->value_node))
			throw CodegenException("A variable should have either a default value or a type annotation");
	}

	[[nodiscard]] inline const std::string &get_name() const { return name; }
	[[nodiscard]] inline const ValueNode *get_value_node() const { return value_node.get(); }
	[[nodiscard]] inline bool is_const() const { return const_flag; }

	OVERRIDE(Value)
private:
	std::string name;
	Ptr<TypeNode> type_node;
	Ptr<ValueNode> value_node;
	bool const_flag;
};

class BlockNode : public StmtNode {
public:
	BlockNode(
		const SourceRange &range,
		std::vector<StmtNode *> stmts
	);

	[[nodiscard]] inline const std::vector<StmtNode *> &get_statements() const {
		return stmts;
	}

	OVERRIDE(Value)

	~BlockNode() override;
private:
	std::vector<StmtNode *> stmts;
	bool has_return_flag;

	friend class ValueNode;
};

class FunctionNode : public DeclNode {
public:
	FunctionNode(
		const SourceRange &range,
		Ptr<PrototypeNode> prototype_node,
		Ptr<BlockNode> body_node
	): ASTNode(range),
	   prototype_node(std::move(prototype_node)),
	   body_node(std::move(body_node)) {}

	[[nodiscard]] inline const std::string &get_name() const { return prototype_node->get_name(); }
	[[nodiscard]] inline const PrototypeNode *get_prototype_node() const {
		return prototype_node.get();
	}
	[[nodiscard]] inline const BlockNode *get_body_node() const { return body_node.get(); }

	OVERRIDE(void)
private:
	Ptr<PrototypeNode> prototype_node;
	Ptr<BlockNode> body_node;
};

class ReturnNode : public StmtNode {
public:
	ReturnNode(
		const SourceRange &range,
		Ptr<ValueNode> value_node
	): StmtNode(range), value_node(std::move(value_node)) {}

	[[nodiscard]] inline const ValueNode *get_value_node() const { return value_node.get(); }

	OVERRIDE(Value)
private:
	Ptr<ValueNode> value_node;
};

class IfNode : public StmtNode {
public:
	IfNode(
		const SourceRange &range,
		Ptr<ValueNode> condition_node,
		Ptr<StmtNode> then_node,
		Ptr<StmtNode> else_node
	): StmtNode(range),
	   condition_node(std::move(condition_node)),
	   then_node(std::move(then_node)),
	   else_node(std::move(else_node)) {}

	[[nodiscard]] inline const ValueNode *get_condition_node() const { return condition_node.get(); }
	[[nodiscard]] inline const StmtNode *get_then_node() const { return then_node.get(); }
	[[nodiscard]] inline const StmtNode *get_else_node() const { return else_node.get(); }

	OVERRIDE(Value)
private:
	Ptr<ValueNode> condition_node;
	Ptr<StmtNode> then_node, else_node;
};

#undef OVERRIDE

} // namespace rin
