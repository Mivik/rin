
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

	[[nodiscard]] virtual bool has_return() const { return false; }
	// TODO move const_eval to codegen?
	virtual Value codegen(Codegen &g, bool const_eval = false) const = 0;

	[[nodiscard]] SourceRange get_source_range() const { return range; }
};

#define OVERRIDE \
    Value codegen(Codegen &g, bool const_eval = false) const override;

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
	enum class Type: uint8_t {
		VAR, VAL, CONST
	};

	VarDeclNode(
		const SourceRange &range,
		std::string name,
		Ptr<ASTNode> type_node,
		Ptr<ASTNode> value_node,
		Type var_type
	): ASTNode(range), name(std::move(name)),
	   type_node(std::move(type_node)),
	   value_node(std::move(value_node)),
	   var_type(var_type) {
		if (!(this->type_node || this->value_node))
			throw CodegenException("A variable should have either a default value or a type annotation");
		if ((var_type == Type::CONST || var_type == Type::VAL) && !this->value_node)
			throw CodegenException("The default value of const variable should be given");
	}

	[[nodiscard]] const std::string &get_name() const { return name; }
	[[nodiscard]] const ASTNode *get_value_node() const { return value_node.get(); }
	[[nodiscard]] Type get_variable_type() const { return var_type; }

	OVERRIDE
private:
	std::string name;
	Ptr<ASTNode> type_node, value_node;
	Type var_type;
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

class BlockNode : public ASTNode {
public:
	BlockNode(
		const SourceRange &range,
		std::vector<Ptr<ASTNode>> stmts
	);

	[[nodiscard]] const std::vector<Ptr<ASTNode>> &get_statements() const { return stmts; }
	[[nodiscard]] bool has_return() const override { return has_return_flag; }

	OVERRIDE
private:
	std::vector<Ptr<ASTNode>> stmts;
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
	   param_names(std::move(param_names)) {}

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

// TODO generic function
// TODO const-evaluated
class FunctionNode : public ASTNode {
public:
	FunctionNode(
		const SourceRange &range,
		std::string name,
		Ptr<FunctionTypeNode> type_node,
		Ptr<BlockNode> body_node
	): ASTNode(range),
	   name(std::move(name)),
	   type_node(std::move(type_node)),
	   body_node(std::move(body_node)) {}

	[[nodiscard]] const std::string &get_name() const { return name; }
	[[nodiscard]] const ASTNode *get_type_node() const { return type_node.get(); }
	[[nodiscard]] const BlockNode *get_body_node() const { return body_node.get(); }

	OVERRIDE
private:
	std::string name;
	Ptr<FunctionTypeNode> type_node;
	Ptr<BlockNode> body_node;
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
