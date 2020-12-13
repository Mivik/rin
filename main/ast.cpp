
#include "ast.h"

namespace rin {

std::string ConstantNode::to_string() const { return str; }

std::string UnaryOpNode::to_string() const {
	std::string str;
#define H(k, s) case k: str = s; break;
	switch (op) {
		H(UAdd, "+") H(USub, "-")
		H(Not, "~") H(LNot, "!")
		default: rin_unreachable("Illegal unary operator: " + token_kind::name(op));
	}
#undef H
	return '(' + str + ' ' + value_node->to_string() + ')';
}

std::string BinOpNode::to_string() const {
	std::string str;
#define H(k, s) case k: str = s; break;
	switch (op) {
		H(Add, '+') H(Sub, '-')
		H(Mul, '*') H(Div, '/') H(Mod, '%')
		H(Shl, "<<") H(Shr, ">>") H(Or, '|') H(And, '&') H(Xor, '^')
		H(Assign, '=') H(AddA, "+=") H(SubA, "-=")
		H(MulA, "*=") H(DivA, "/=") H(ModA, "%=")
		H(ShlA, "<<=") H(ShrA, ">>=")
		H(OrA, "|=") H(AndA, "&=") H(XorA, "^=")
		H(Lt, '<') H(Gt, '>') H(Le, "<=") H(Ge, ">=")
		H(Eq, "==") H(Neq, "!=")
		default: rin_unreachable("Illegal binary operator: " + token_kind::name(op));
	}
#undef H
	return '(' + lhs_node->to_string() + ' ' + str + ' ' + rhs_node->to_string() + ')';
}

} // namespace rin
