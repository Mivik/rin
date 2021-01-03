
#pragma once

#include <string>

namespace rin {

class MemoryBuffer;

enum TokenKind {
	Eof, Newline,
	Identifier,
	Number, True, False,
	String,
	Comment, MLComment,
	LPar, RPar, LBracket, RBracket, LBrace, RBrace,
	Colon, Semicolon, Comma, Period, Arrow,
	// Keywords
	Const, Do, Else, Enum, Fn, For, If, In, Is, Let, Return, Var, When, While,
	// Binary operators
	Add, Sub, Mul, Div, Mod, Shl, Shr, Or, And, Not, Xor, LOr, LAnd, LNot,
	Assign, AddA, SubA, MulA, DivA, ModA, ShlA, ShrA, OrA, AndA, XorA,
	Lt, Gt, Le, Ge, Eq, Neq,
	// Unary operators (which cannot be generated directly from lexer but from parser)
	UAdd, USub,
};

namespace token_kind {

std::string name(TokenKind kind);

inline TokenKind as_unary_op(TokenKind kind) {
	switch (kind) {
		case Add:
			return UAdd;
		case Sub:
			return USub;
		default:
			return kind;
	}
}

inline bool is_unary_op(TokenKind kind) {
	switch (kind) {
		case UAdd:
		case USub:
		case Not:
		case LNot:
			return true;
		default:
			return false;
	}
}

inline bool is_binary_op(TokenKind kind) {
	switch (kind) {
		case Add:
		case Sub:
		case Mul:
		case Div:
		case Mod:
		case Shl:
		case Shr:
		case Or:
		case And:
		case Xor:
		case Assign:
		case AddA:
		case SubA:
		case MulA:
		case DivA:
		case ModA:
		case ShlA:
		case ShrA:
		case OrA:
		case AndA:
		case XorA:
		case Lt:
		case Gt:
		case Le:
		case Ge:
		case Eq:
		case Neq:
			return true;
		default:
			return false;
	}
}

} // namespace token_kind

struct SourceRange {
	static inline SourceRange empty() { return SourceRange(0, 0); }

	size_t begin, end;
	explicit SourceRange(size_t pos):
		begin(pos), end(pos) {}
	SourceRange(size_t begin, size_t end):
		begin(begin), end(end) {}
	[[nodiscard]] inline bool is_empty() const { return begin >= end; }
	inline SourceRange operator+(const SourceRange &other) const {
		return { begin, other.end };
	}
	inline bool operator==(const SourceRange &other) const {
		return begin == other.begin && end == other.end;
	}
	inline bool operator!=(const SourceRange &other) const {
		return begin != other.begin || end != other.end;
	}
	[[nodiscard]] inline std::string to_string() const {
		return '[' + std::to_string(begin) + ", " + std::to_string(end) + ')';
	}
};

struct Token {
	TokenKind kind;
	SourceRange range;
	Token(TokenKind kind, const SourceRange &range):
		kind(kind), range(range) {}
	[[nodiscard]] inline std::string name() const { return token_kind::name(kind); }
	[[nodiscard]] std::string content(const MemoryBuffer &buffer) const;
	[[nodiscard]] inline std::string info(const MemoryBuffer &buffer) const {
		auto ret = '[' + name() + ']';
		if (!range.is_empty()) ret = ret + " \"" + content(buffer) + '"';
		return ret;
	}
	inline explicit operator bool() const { return kind != Eof; }
};

} // namespace rin
