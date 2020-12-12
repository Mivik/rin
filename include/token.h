
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
	Lt, Gt, Le, Ge, Eq, Neq
};

namespace token_kind {

std::string name(TokenKind kind);

inline bool is_unary(TokenKind kind) {
	switch (kind) {
		case Add: case Sub:
		case Not: case LNot:
			return true;
		default: return false;
	}
}

} // namespace token_kind

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
	inline std::string name() const { return token_kind::name(kind); }
	std::string content(const MemoryBuffer &buffer) const;
	inline std::string info(const MemoryBuffer &buffer) const {
		return "[" + name() + "] \"" + content(buffer) + "\"";
	}
	inline operator bool() const { return kind != Eof; }
};

} // namespace rin
