
#pragma once

#include <string>

namespace rin {

enum class TokenKind {
#define TOKEN(name) name,

#include "token.def"
};

namespace token_kind {

std::string name(TokenKind kind);

inline TokenKind as_unary_op(TokenKind kind) {
	switch (kind) {
		case TokenKind::Add:
			return TokenKind::UAdd;
		case TokenKind::Sub:
			return TokenKind::USub;
		default:
			return kind;
	}
}

inline bool is_unary_op(TokenKind kind) {
	switch (kind) {
#define TOKEN_UNARY_OP(name) case TokenKind::name:

#include "token.def"

		return true;
		default:
			return false;
	}
}

inline bool is_binary_op(TokenKind kind) {
	// TODO use helper include file to simplify this
	switch (kind) {
#define TOKEN_BINARY_OP(name) case TokenKind::name:

#include "token.def"

		return true;
		default:
			return false;
	}
}

} // namespace token_kind

class SourceRange {
public:
	static inline SourceRange make_empty() { return SourceRange(0, 0); }

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
		auto ret = '[' + name() + ']';
		if (!range.empty()) ret = ret + " \"" + std::string(content(buffer)) + '"';
		return ret;
	}

	explicit operator bool() const { return kind != TokenKind::Eof; }

	TokenKind kind;
	SourceRange range;
};

} // namespace rin
