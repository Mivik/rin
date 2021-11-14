
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
	virtual void preserve_reference() {}
	// TODO move const_eval to codegen?
	virtual Value codegen(Codegen &g) const = 0;

	[[nodiscard]] SourceRange get_source_range() const { return range; }
};

#define OVERRIDE \
    Value codegen(Codegen &g) const override;

class PhonyASTNode final : public ASTNode {
public:
	explicit PhonyASTNode(Value value): ASTNode(SourceRange(-1)), value(value) {}

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

	void preserve_reference() override { preserve_ref = true; }

	OVERRIDE
private:
	Ptr<ASTNode> lhs_node, rhs_node;
	TokenKind op;
	bool preserve_ref = false;
};

class ValueNode final : public ASTNode {
public:
	ValueNode(const SourceRange &range, std::string name):
		ASTNode(range), name(std::move(name)) {}
	ValueNode(const Token &token, const Reader &input):
		ASTNode(token.range), name(input.substr(token.range)) {}

	[[nodiscard]] const std::string &get_name() const { return name; }

	void preserve_reference() override { preserve_ref = true; }

	OVERRIDE
private:
	std::string name;
	bool preserve_ref = false;
};

class ArrayTypeNode final : public ASTNode {
public:
	ArrayTypeNode(
		const SourceRange &range,
		Ptr<ASTNode> sub_type_node,
		Ptr<ASTNode> length_node
	): ASTNode(range),
	   sub_type_node(std::move(sub_type_node)),
	   length_node(std::move(length_node)) {}

	[[nodiscard]] const ASTNode *get_sub_type_node() const { return sub_type_node.get(); }
	[[nodiscard]] const ASTNode *get_length_node() const { return length_node.get(); }

	OVERRIDE
private:
	Ptr<ASTNode> sub_type_node, length_node;
};

class VarDeclNode : public ASTNode {
public:
	VarDeclNode(
		const SourceRange &range,
		std::string name,
		Ptr<ASTNode> type_node,
		Ptr<ASTNode> value_node,
		bool is_mutable,
		bool is_inline
	);

	[[nodiscard]] const std::string &get_name() const { return name; }
	[[nodiscard]] const ASTNode *get_value_node() const { return value_node.get(); }
	[[nodiscard]] bool is_mutable() const { return mutable_flag; }
	[[nodiscard]] bool is_inline() const { return inline_flag; }

	OVERRIDE
private:
	std::string name;
	Ptr<ASTNode> type_node, value_node;
	bool mutable_flag, inline_flag;
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

	void preserve_reference() override { preserve_ref = true; }

	OVERRIDE
private:
	std::string name;
	Ptr<ASTNode> receiver_node;
	std::vector<Ptr<ASTNode>> argument_nodes;
	bool preserve_ref = false;

	friend class BinOpNode; // call with receiver
};

class BlockNode final : public ASTNode {
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

class FunctionTypeNode final : public ASTNode {
public:
	FunctionTypeNode(
		const SourceRange &range,
		std::vector<std::pair<std::string, Ptr<ASTNode>>> template_parameters,
		Ptr<ASTNode> receiver_type_node,
		Ptr<ASTNode> result_type_node,
		std::vector<Ptr<ASTNode>> parameter_type_nodes,
		std::vector<std::string> parameter_names
	): ASTNode(range),
	   template_parameters(std::move(template_parameters)),
	   receiver_type_node(std::move(receiver_type_node)),
	   result_type_node(std::move(result_type_node)),
	   parameter_type_nodes(std::move(parameter_type_nodes)),
	   parameter_names(std::move(parameter_names)) {
		assert_unique(parameter_names);
	}

	[[nodiscard]] const std::vector<std::pair<std::string, Ptr<ASTNode>>> &get_template_parameters() const {
		return template_parameters;
	}
	[[nodiscard]] ASTNode *get_receiver_type_node() const { return receiver_type_node.get(); }
	[[nodiscard]] ASTNode *get_result_type_node() const { return result_type_node.get(); }
	[[nodiscard]] std::vector<ASTNode *> get_parameter_type_nodes() const {
		std::vector<ASTNode *> res(parameter_type_nodes.size());
		for (size_t i = 0; i < res.size(); ++i) res[i] = parameter_type_nodes[i].get();
		return res;
	}
	[[nodiscard]] const std::vector<std::string> &get_parameter_names() const { return parameter_names; }

	[[nodiscard]] Type *get_receiver_type(Codegen &g) const;
	[[nodiscard]] Type *get_result_type(Codegen &g) const;
	[[nodiscard]] std::vector<Value> get_parameter_types(Codegen &g) const;

	OVERRIDE
private:
	std::vector<std::pair<std::string, Ptr<ASTNode>>> template_parameters;
	Ptr<ASTNode> receiver_type_node, result_type_node;
	std::vector<Ptr<ASTNode>> parameter_type_nodes;
	std::vector<std::string> parameter_names;
};

class StructNode final : public ASTNode {
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

class StructValueNode final : public ASTNode {
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

class TupleNode final : public ASTNode {
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
class TupleValueNode final : public ASTNode {
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

class TopLevelNode final : public ASTNode {
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
class FunctionNode final : public DeclNode {
public:
	FunctionNode(
		const SourceRange &range,
		std::string name,
		Ptr<FunctionTypeNode> type_node,
		Ptr<ASTNode> content_node,
		bool inline_flag
	): DeclNode(range),
	   name(std::move(name)),
	   type_node(std::move(type_node)),
	   content_node(std::move(content_node)),
	   function_object(nullptr),
	   type(nullptr),
	   inline_flag(inline_flag) {}

	[[nodiscard]] const FunctionTypeNode *get_type_node() const { return type_node.get(); }
	[[nodiscard]] const ASTNode *get_content_node() const { return content_node.get(); }
	[[nodiscard]] Function *get_function_object() const { return function_object; }
	[[nodiscard]] bool is_inline() const { return inline_flag; }

	OVERRIDE

	void declare(Codegen &g) override;

	std::string name;
private:
	Ptr<FunctionTypeNode> type_node;
	Ptr<ASTNode> content_node;

	// Initialize at declaration
	Function *function_object;
	Type::Function *type;

	bool inline_flag;
};

class GlobalVarDeclNode final : public DeclNode {
public:
	GlobalVarDeclNode(
		const SourceRange &range,
		std::string name,
		Ptr<ASTNode> type_node,
		Ptr<ASTNode> value_node,
		bool is_mutable,
		bool is_inline
	): DeclNode(range),
	   name(std::move(name)),
	   type_node(std::move(type_node)),
	   value_node(std::move(value_node)),
	   mutable_flag(is_mutable), inline_flag(is_inline) {}

	[[nodiscard]] const std::string &get_name() const { return name; }
	[[nodiscard]] const ASTNode *get_value_node() const { return value_node.get(); }
	[[nodiscard]] bool is_mutable() const { return mutable_flag; }
	[[nodiscard]] bool is_inline() const { return inline_flag; }

	OVERRIDE

	void declare(Codegen &g) override;
private:
	std::string name;
	Ptr<ASTNode> type_node, value_node;
	bool mutable_flag, inline_flag;
	Value initial_value, global_ref;
};

class ReturnNode final : public ASTNode {
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

class IfNode final : public ASTNode {
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

class ConceptNode final : public ASTNode {
public:
	ConceptNode(
		const SourceRange &range,
		std::vector<std::pair<Ptr<FunctionTypeNode>, std::string>> function_nodes
	): ASTNode(range),
	   function_nodes(std::move(function_nodes)) {}

	OVERRIDE
private:
	std::vector<std::pair<Ptr<FunctionTypeNode>, std::string>> function_nodes;
};

class ImplNode final : public DeclNode {
public:
	ImplNode(
		const SourceRange &range,
		Ptr<ASTNode> type_node,
		Ptr<ASTNode> concept_node,
		std::vector<Ptr<FunctionNode>> function_nodes
	): DeclNode(range),
	   type_node(std::move(type_node)),
	   concept_node(std::move(concept_node)),
	   function_nodes(std::move(function_nodes)) {}

	[[nodiscard]] const ASTNode *get_type_node() const { return type_node.get(); }
	[[nodiscard]] const ASTNode *get_concept_node() const { return concept_node.get(); }
	[[nodiscard]] const std::vector<Ptr<FunctionNode>> &get_function_nodes() const { return function_nodes; }

	void declare(Codegen &g) override {}

	OVERRIDE
private:
	Ptr<ASTNode> type_node, concept_node;
	std::vector<Ptr<FunctionNode>> function_nodes;
};

#undef OVERRIDE

} // namespace rin
