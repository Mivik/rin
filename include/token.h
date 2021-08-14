
#pragma once

#include <limits>
#include <string>

namespace rin {

enum class TokenKind {
#define TOKEN(name) name,

#include "token.def"
};

namespace token_kind {

using K = TokenKind;

std::string name(K kind);

inline K as_unary_op(K kind) {
	switch (kind) {
		case K::Add:
			return K::UAdd;
		case K::Sub:
			return K::USub;
		case K::Mul:
			return K::Pointer;
		case K::LAnd:
			return K::Ref;
		default:
			return kind;
	}
}

inline bool is_unary_op(K kind) {
	switch (kind) {
#define TOKEN_UNARY_OP(name) case TokenKind::name:

#include "token.def"

			return true;
		default:
			return false;
	}
}

inline bool is_binary_op(K kind) {
	switch (kind) {
#define TOKEN_BINARY_OP(name) case TokenKind::name:

#include "token.def"

			return true;
		default:
			return false;
	}
}

inline int precedence_of(K op) {
#define H(n) case TokenKind::n
	switch (op) {
		H(LBracket): // pointer subscript
		H(UAdd):
		H(USub):
		H(Not):
		H(LNot):
		// TODO think over it
		H(Pointer):
		H(Ref):
			return 2;
		H(Mul):
		H(Div):
		H(Mod):
			return 3;
		H(Add):
		H(Sub):
			return 4;
		H(Shl):
		H(Shr):
			return 5;
		H(Lt):
		H(Le):
		H(Gt):
		H(Ge):
			return 7;
		H(Eq):
		H(Neq):
			return 8;
		H(And):
			return 9;
		H(Xor):
			return 10;
		H(Or):
			return 11;
		H(LAnd):
			return 12;
		H(LOr):
			return 13;
		H(Assign):
		H(AddA):
		H(SubA):
		H(MulA):
		H(DivA):
		H(ModA):
		H(ShlA):
		H(ShrA):
		H(AndA):
		H(OrA):
		H(XorA):
			return 14;
		default:
			return std::numeric_limits<int>::max();
	}
#undef H
}

} // namespace token_kind

class SourceRange {
public:
	static SourceRange make_empty() { return SourceRange(0, 0); }

	explicit SourceRange(size_t pos): begin(pos), end(pos) {}
	SourceRange(size_t begin, size_t end):
		begin(begin), end(end) {}
	[[nodiscard]] bool empty() const { return begin >= end; }
	[[nodiscard]] size_t size() const { return end - begin; }

	SourceRange operator+(const SourceRange &other) const {
		return { begin, other.end };
	}
	bool operator==(const SourceRange &other) const {
		return begin == other.begin && end == other.end;
	}
	bool operator!=(const SourceRange &other) const {
		return begin != other.begin || end != other.end;
	}
	[[nodiscard]] std::string to_string() const {
		return '[' + std::to_string(begin) + ", " + std::to_string(end) + ')';
	}

	const size_t begin, end;
};

class Token {
public:
	Token(TokenKind kind, const SourceRange &range):
		kind(kind), range(range) {}
	[[nodiscard]] std::string name() const { return token_kind::name(kind); }

	[[nodiscard]] std::string_view content(std::string_view buffer) const;
	[[nodiscard]] std::string info(std::string_view buffer) const {
		auto res = '[' + name() + ']';
		if (!range.empty()) res = res + " \"" + std::string(content(buffer)) + '"';
		return res;
	}

	explicit operator bool() const { return kind != TokenKind::Eof; }

	TokenKind kind;
	SourceRange range;
};

} // namespace rin
