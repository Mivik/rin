
#pragma once

#include "codegen.h"
#include "lexer.h"
#include "token.h"
#include "value.h"

namespace rin {

class ParseException;

class ASTNode {
protected:
	explicit ASTNode(const SourceRange &range): range(range) {}

	SourceRange range;
public:
	virtual ~ASTNode() = default;

	[[nodiscard]] virtual bool has_return() const { return false; }
	// TODO move const_eval to codegen?
	virtual Value codegen(Codegen &g) const = 0;

	[[nodiscard]] SourceRange get_source_range() const { return range; }
};

#define OVERRIDE \
    Value codegen(Codegen &g) const override;

class PhonyASTNode final : public ASTNode {
public:
	PhonyASTNode(Value value): ASTNode(SourceRange(-1)), value(value) {}

	[[nodiscard]] Value get_value() const { return value; }

	Value codegen(Codegen &) const override { return value; }
private:
	Value value;
};

class ConstantNode final : public ASTNode {
public:
	ConstantNode(const Token &token, const Reader &input):
		ASTNode(token.range), content(input.substr(token.range)) {}

	[[nodiscard]] const std::string &get_content() const { return content; }

	OVERRIDE
private:
	std::string content;
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
	ValueNode(const SourceRange &range, std::string name): ASTNode(range), name(std::move(name)) {}
	ValueNode(const Token &token, const Reader &input):
		ASTNode(token.range), name(input.substr(token.range)) {}

	[[nodiscard]] const std::string &get_name() const { return name; }

	OVERRIDE
private:
	std::string name;
};

class VarDeclNode : public ASTNode {
public:
	VarDeclNode(
		const SourceRange &range,
		std::string name,
		Ptr<ASTNode> type_node,
		Ptr<ASTNode> value_node,
		bool is_mutable,
		bool is_const
	);

	[[nodiscard]] const std::string &get_name() const { return name; }
	[[nodiscard]] const ASTNode *get_value_node() const { return value_node.get(); }
	[[nodiscard]] bool is_mutable() const { return mutable_flag; }
	[[nodiscard]] bool is_const() const { return const_flag; }

	OVERRIDE
private:
	std::string name;
	Ptr<ASTNode> type_node, value_node;
	bool mutable_flag, const_flag;
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

	friend class BinOpNode; // call with receiver
};

class BlockNode : public ASTNode {
public:
	BlockNode(
		const SourceRange &range,
		std::vector<Ptr<ASTNode>> stmts
	);

	[[nodiscard]] const std::vector<Ptr<ASTNode>> &get_statements() const { return stmt_nodes; }
	[[nodiscard]] bool has_return() const override { return has_return_flag; }

	OVERRIDE
private:
	std::vector<Ptr<ASTNode>> stmt_nodes;
	bool has_return_flag;
};

class FunctionTypeNode : public ASTNode {
public:
	FunctionTypeNode(
		const SourceRange &range,
		Ptr<ASTNode> receiver_type_node,
		Ptr<ASTNode> result_type_node,
		std::vector<Ptr<ASTNode>> param_type_nodes,
		std::vector<std::string> param_names
	): ASTNode(range),
	   receiver_type_node(std::move(receiver_type_node)),
	   result_type_node(std::move(result_type_node)),
	   param_type_nodes(std::move(param_type_nodes)),
	   param_names(std::move(param_names)) {
		assert_unique(param_names);
	}

	[[nodiscard]] ASTNode *get_receiver_type_node() const { return receiver_type_node.get(); }
	[[nodiscard]] ASTNode *get_result_type_node() const { return result_type_node.get(); }
	[[nodiscard]] const std::vector<Ptr<ASTNode>> &get_parameter_type_nodes() const { return param_type_nodes; }
	[[nodiscard]] const std::vector<std::string> &get_parameter_names() const { return param_names; }


	OVERRIDE
private:
	Ptr<ASTNode> receiver_type_node, result_type_node;
	std::vector<Ptr<ASTNode>> param_type_nodes;
	std::vector<std::string> param_names;
};

class StructNode : public ASTNode {
public:
	StructNode(
		const SourceRange &range,
		std::vector<std::string> field_names,
		std::vector<Ptr<ASTNode>> field_types
	): ASTNode(range),
	   field_names(std::move(field_names)),
	   field_types(std::move(field_types)) {
		assert(this->field_names.size() == this->field_types.size());
		assert_unique(field_names);
	}

	[[nodiscard]] const std::vector<std::string> &get_field_names() const { return field_names; }
	[[nodiscard]] const std::vector<Ptr<ASTNode>> &get_field_types() const { return field_types; }

	OVERRIDE
private:
	std::vector<std::string> field_names;
	std::vector<Ptr<ASTNode>> field_types;
};

class StructValueNode : public ASTNode {
public:
	StructValueNode(
		const SourceRange &range,
		Ptr<ASTNode> type_node,
		std::vector<Ptr<ASTNode>> field_nodes
	): ASTNode(range),
	   type_node(std::move(type_node)),
	   field_nodes(std::move(field_nodes)) {}

	[[nodiscard]] const ASTNode *get_type_node() const { return type_node.get(); }
	[[nodiscard]] const std::vector<Ptr<ASTNode>> &get_field_nodes() const { return field_nodes; }

	OVERRIDE
private:
	Ptr<ASTNode> type_node;
	std::vector<Ptr<ASTNode>> field_nodes;
};

class TupleNode : public ASTNode {
public:
	TupleNode(
		const SourceRange &range,
		std::vector<Ptr<ASTNode>> element_types
	): ASTNode(range),
	   element_types(std::move(element_types)) {}

	[[nodiscard]] const std::vector<Ptr<ASTNode>> &get_element_types() const { return element_types; }

	OVERRIDE
private:
	std::vector<Ptr<ASTNode>> element_types;
};

// TODO reference type?
class TupleValueNode : public ASTNode {
public:
	TupleValueNode(
		const SourceRange &range,
		std::vector<Ptr<ASTNode>> element_nodes
	): ASTNode(range),
	   element_nodes(std::move(element_nodes)) {}

	[[nodiscard]] const std::vector<Ptr<ASTNode>> &get_element_nodes() const { return element_nodes; }

	OVERRIDE
private:
	std::vector<Ptr<ASTNode>> element_nodes;
};

// Top-level declarations
class DeclNode : public ASTNode {
public:
	// Called only once
	virtual void declare(Codegen &g) = 0;
protected:
	explicit DeclNode(const SourceRange &range): ASTNode(range) {}
};

class TopLevelNode : public ASTNode {
public:
	TopLevelNode(
		const SourceRange &range,
		std::vector<Ptr<DeclNode>> children
	): ASTNode(range),
	   child_nodes(std::move(children)) {}

	[[nodiscard]] const std::vector<Ptr<DeclNode>> &get_child_nodes() const { return child_nodes; }

	OVERRIDE
private:
	std::vector<Ptr<DeclNode>> child_nodes;
};

// TODO generic function
// TODO const-evaluate
class FunctionNode : public DeclNode {
public:
	FunctionNode(
		const SourceRange &range,
		std::string name,
		Ptr<FunctionTypeNode> type_node,
		Ptr<BlockNode> body_node
	): DeclNode(range),
	   name(std::move(name)),
	   type_node(std::move(type_node)),
	   body_node(std::move(body_node)),
	   function_object(nullptr),
	   type(nullptr) {}

	[[nodiscard]] const std::string &get_name() const { return name; }
	[[nodiscard]] const ASTNode *get_type_node() const { return type_node.get(); }
	[[nodiscard]] const BlockNode *get_body_node() const { return body_node.get(); }

	OVERRIDE

	void declare(Codegen &g) override;
private:
	std::string name;
	Ptr<FunctionTypeNode> type_node;
	Ptr<BlockNode> body_node;

	// Initialize at declaration
	Function::Static *function_object;
	Type::Function *type;
};

class GlobalVarDeclNode : public DeclNode {
public:
	GlobalVarDeclNode(
		const SourceRange &range,
		std::string name,
		Ptr<ASTNode> type_node,
		Ptr<ASTNode> value_node,
		bool is_mutable,
		bool is_const
	): DeclNode(range),
	   name(std::move(name)),
	   type_node(std::move(type_node)),
	   value_node(std::move(value_node)),
	   mutable_flag(is_mutable), const_flag(is_const) {}

	[[nodiscard]] const std::string &get_name() const { return name; }
	[[nodiscard]] const ASTNode *get_value_node() const { return value_node.get(); }
	[[nodiscard]] bool is_mutable() const { return mutable_flag; }
	[[nodiscard]] bool is_const() const { return const_flag; }

	OVERRIDE

	void declare(Codegen &g) override;
private:
	std::string name;
	Ptr<ASTNode> type_node, value_node;
	bool mutable_flag, const_flag;
	Value initial_value, global_ref;
};

class ReturnNode : public ASTNode {
public:
	ReturnNode(
		const SourceRange &range,
		Ptr<ASTNode> value_node
	): ASTNode(range), value_node(std::move(value_node)) {}

	[[nodiscard]] const ASTNode *get_value_node() const { return value_node.get(); }
	[[nodiscard]] bool has_return() const override { return true; }

	OVERRIDE
private:
	Ptr<ASTNode> value_node;
};

class IfNode : public ASTNode {
public:
	IfNode(
		const SourceRange &range,
		Ptr<ASTNode> condition_node,
		Ptr<ASTNode> then_node,
		Ptr<ASTNode> else_node
	): ASTNode(range),
	   condition_node(std::move(condition_node)),
	   then_node(std::move(then_node)),
	   else_node(std::move(else_node)),
	   has_return_flag(this->else_node && this->then_node->has_return() && this->else_node->has_return()) {}

	[[nodiscard]] const ASTNode *get_condition_node() const { return condition_node.get(); }
	[[nodiscard]] const ASTNode *get_then_node() const { return then_node.get(); }
	[[nodiscard]] const ASTNode *get_else_node() const { return else_node.get(); }
	[[nodiscard]] bool has_return() const override { return has_return_flag; }

	OVERRIDE
private:
	Ptr<ASTNode> condition_node, then_node, else_node;
	bool has_return_flag;
};

#undef OVERRIDE

} // namespace rin
