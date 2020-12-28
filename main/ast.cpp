
#include "ast.h"

namespace rin {

bool ValueNode::has_return() const {
	if (dynamic_cast<const ReturnNode *>(this)) return true;
	if (auto block_node = dynamic_cast<const BlockNode *>(this))
		return block_node->has_return_flag;
	return false;
}

std::string ConstantNode::to_string() const { return str; }

std::string UnaryOpNode::to_string() const {
	std::string str;
#define H(k, s) case k: str = s; break;
	switch (op) {
		H(UAdd, "+")
		H(USub, "-")
		H(Not, "~")
		H(LNot, "!")
		default:
			rin_unreachable("Illegal unary operator: " + token_kind::name(op));
	}
#undef H
	return '(' + str + ' ' + value_node->to_string() + ')';
}

std::string BinOpNode::to_string() const {
	std::string str;
#define H(k, s) case k: str = s; break;
	switch (op) {
		H(Add, '+')
		H(Sub, '-')
		H(Mul, '*')
		H(Div, '/')
		H(Mod, '%')
		H(Shl, "<<")
		H(Shr, ">>")
		H(Or, '|')
		H(And, '&')
		H(Xor, '^')
		H(Assign, '=')
		H(AddA, "+=")
		H(SubA, "-=")
		H(MulA, "*=")
		H(DivA, "/=")
		H(ModA, "%=")
		H(ShlA, "<<=")
		H(ShrA, ">>=")
		H(OrA, "|=")
		H(AndA, "&=")
		H(XorA, "^=")
		H(Lt, '<')
		H(Gt, '>')
		H(Le, "<=")
		H(Ge, ">=")
		H(Eq, "==")
		H(Neq, "!=")
		default:
			rin_unreachable("Illegal binary operator: " + token_kind::name(op));
	}
#undef H
	return '(' + lhs_node->to_string() + ' ' + str + ' ' + rhs_node->to_string() + ')';
}

std::string NamedValueNode::to_string() const { return name; }

std::string NamedTypeNode::to_string() const { return name; }

std::string ArrayTypeNode::to_string() const {
	return '[' + element_type_node->to_string() + ", " + std::to_string(size) + ']';
}

std::string PointerTypeNode::to_string() const {
	return (const_flag? "*const ": "*") + sub_type_node->to_string();
}

std::string RefTypeNode::to_string() const {
	return (const_flag? "&const ": "&") + sub_type_node->to_string();
}

std::string FunctionTypeNode::to_string() const {
	std::string ret;
	if (receiver_type_node) {
		ret += receiver_type_node->to_string();
		ret += '.';
	}
	ret += '(';
	for (size_t i = 0; i < param_type_nodes.size(); ++i) {
		ret += param_type_nodes[i]->to_string();
		if (i != param_type_nodes.size() - 1) ret += ", ";
	}
	ret += ") -> ";
	ret += result_type_node->to_string();
	return ret;
}

FunctionTypeNode::~FunctionTypeNode() {
	for (auto param : param_type_nodes)
		delete param;
}

std::string VarDeclNode::to_string() const {
	std::string ret = "let";
	if (!const_flag) ret += '*';
	ret += ' ';
	ret += name;
	ret += " = ";
	ret += value_node->to_string();
	return ret;
}

BlockNode::BlockNode(const SourceRange &range, std::vector<StmtNode *> stmts):
	StmtNode(range),
	stmts(std::move(stmts)) {
	auto iter = stmts.begin();
	while (iter != stmts.end() && !dynamic_cast<ReturnNode *>(*iter))
		++iter;
	// TODO Warning here?
	if (iter != stmts.end())
		stmts.erase(++iter, stmts.end());
	has_return_flag = stmts.empty() || stmts.back()->has_return();
}

std::string BlockNode::to_string() const {
	if (stmts.empty()) return "{}";
	std::string ret = "{", tmp;
	for (size_t i = 0; i < stmts.size(); ++i) {
		tmp += stmts[i]->to_string();
		if (i != stmts.size() - 1) tmp += '\n';
	}
	ret += add_indent(tmp);
	ret += "\n}";
	return ret;
}

BlockNode::~BlockNode() {
	for (auto stmt : stmts)
		delete stmt;
}

std::string PrototypeNode::to_string() const {
	std::string ret = "fn ";
	if (auto receiver_type = type_node->get_receiver_type_node()) {
		ret += receiver_type->to_string();
		ret += '.';
	}
	ret += name;
	ret += '(';
	const auto &param_types = type_node->get_parameter_type_nodes();
	for (size_t i = 0; i < param_names.size(); ++i) {
		ret += param_types[i]->to_string();
		ret += ' ';
		ret += param_names[i];
		if (i != param_names.size() - 1) ret += ", ";
	}
	ret += ')';
	if (auto result_type = type_node->get_result_type_node()) {
		ret += ": ";
		ret += result_type->to_string();
	}
	return ret;
}

std::string FunctionNode::to_string() const {
	return
		prototype_node->to_string() + ' ' +
		body_node->to_string();
}

std::string ReturnNode::to_string() const {
	return "return " + value_node->to_string();
}

std::string IfNode::to_string() const {
	std::string ret = "if (";
	ret += condition_node->to_string();
	ret += ") ";
	ret += then_node->to_string();
	if (else_node) {
		ret += " else ";
		ret += else_node->to_string();
	}
	return ret;
}

} // namespace rin
