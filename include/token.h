
#pragma once

#include <string>

namespace rin {

class MemoryBuffer;

enum TokenKind {
	Eof,
	Identifier,
	Number, True, False,
	String,
	Comment, MLComment,
	LPar, RPar, LBracket, RBracket, LBrace, RBrace,
	Colon, Comma, Period,
	// Keywords
	Else, Enum, Fn, For, If, In, Is, Let, Return, Var, When,
	// Binary operators
	Add, Sub, Mul, Div, Mod, Shl, Shr, Or, And, Not, Xor, LOr, LAnd, LNot,
	Assign, AddA, SubA, MulA, DivA, ModA, ShlA, ShrA, OrA, AndA, XorA,
	Lt, Gt, Le, Ge, Eq, Neq,
};

inline std::string token_kind_name(TokenKind kind) {
#define H(n) case n: return #n;
	switch (kind) {
		H(Eof)
		H(Identifier)
		H(Number) H(True) H(False)
		H(String)
		H(Comment) H(MLComment)
		H(LPar) H(RPar) H(LBracket) H(RBracket) H(LBrace) H(RBrace)
		H(Colon) H(Comma) H(Period)
		// Keywords
		H(Else) H(Enum) H(Fn) H(For) H(If) H(In)
		H(Is) H(Let) H(Return) H(Var) H(When)
		// Operators
		H(Add) H(Sub) H(Mul) H(Div) H(Mod) H(Shl)
		H(Shr) H(Or) H(And) H(Not) H(Xor) H(LOr)
		H(LAnd) H(LNot) H(Assign) H(AddA) H(SubA)
		H(MulA) H(DivA) H(ModA) H(ShlA) H(ShrA) H(OrA)
		H(AndA) H(XorA) H(Lt) H(Gt) H(Le) H(Ge) H(Eq) H(Neq)
		default: throw (std::string)"Illegal token kind: " + std::to_string(kind);
	}
#undef H
}

struct SourceRange {
	size_t begin, end;
	SourceRange(size_t begin, size_t end):
		begin(begin), end(end) {}
	static inline SourceRange empty() { return SourceRange(1, 0); }
};

struct Token {
	TokenKind kind;
	SourceRange range;
	Token(TokenKind kind, const SourceRange &range):
		kind(kind), range(range) {}
	inline std::string name() const { return token_kind_name(kind); }
	std::string content(const MemoryBuffer &buffer) const;
	inline operator bool() const { return kind != Eof; }
};

} // namespace rin
