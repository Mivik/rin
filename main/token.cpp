
#include "lexer.h"
#include "token.h"

namespace rin {

std::string Token::content(const MemoryBuffer &buffer) const {
	return buffer.substr(range);
}

namespace token_kind {

std::string name(TokenKind kind) {
#define H(n) case n: return #n;
	switch (kind) {
		H(Eof)
		H(Identifier)
		H(Number) H(True) H(False)
		H(String)
		H(Comment) H(MLComment)
		H(LPar) H(RPar) H(LBracket) H(RBracket) H(LBrace) H(RBrace)
		H(Colon) H(Semicolon) H(Comma) H(Period) H(Arrow)
		// Keywords
		H(Const) H(Else) H(Enum) H(Fn) H(For) H(If)
		H(In) H(Is) H(Let) H(Return) H(Var) H(When)
		// Operators
		H(Add) H(Sub) H(Mul) H(Div) H(Mod) H(Shl)
		H(Shr) H(Or) H(And) H(Not) H(Xor) H(LOr)
		H(LAnd) H(LNot) H(Assign) H(AddA) H(SubA)
		H(MulA) H(DivA) H(ModA) H(ShlA) H(ShrA) H(OrA)
		H(AndA) H(XorA) H(Lt) H(Gt) H(Le) H(Ge) H(Eq) H(Neq)
		// Unary operators
		H(UAdd) H(USub)
		default: throw "Illegal token kind: " + std::to_string(kind);
	}
#undef H
}

} // namespace token_kind

} // namespace rin
