
#pragma once

#include <cstring>
#include <deque>
#include <exception>
#include <memory>
#include <utility>
#include <vector>

#include "token.h"

namespace rin {

class SourceRange;

class Token;

class LexException : public std::exception {
public:
	[[nodiscard]] const char *what() const noexcept override { return msg.data(); }

	explicit LexException(std::string msg): msg(std::move(msg)) {}
private:
	std::string msg;
};

class MemoryBuffer {
public:
	const char *const begin, *const end;

	MemoryBuffer(const char *begin, const char *end):
		begin(begin), end(end) {}
	explicit MemoryBuffer(const char *str):
		begin(str), end(str + strlen(str)) {}

	[[nodiscard]] std::string substr(const SourceRange &range) const;
};

class Reader {
public:
	explicit Reader(const MemoryBuffer &buffer):
		buffer(buffer), ptr(buffer.begin) {}

	[[nodiscard]] inline const MemoryBuffer &get_buffer() const { return buffer; }
	[[nodiscard]] inline size_t position() const { return ptr - buffer.begin; }
	[[nodiscard]] inline char peek() const { return (ptr >= buffer.end)? (char) EOF: *ptr; }
	inline char take() {
		const char r = peek();
		++ptr;
		return r;
	}
	void rewind(size_t count = 1);
	inline void rewind_to(size_t pos) { ptr = buffer.begin + pos; }
private:
	MemoryBuffer buffer;
	const char *ptr;
};

class Lexer {
public:
	explicit Lexer(MemoryBuffer buffer):
		input(buffer), has_newline(false) {}
	[[nodiscard]] const MemoryBuffer &get_buffer() const { return input.get_buffer(); }
	Token peek(bool ignore_comment = true);
	Token take(bool ignore_comment = true);
	inline size_t position() {
		if (buffer.empty()) return input.position();
		return buffer.front().range.begin;
	}
	inline void rewind_to(size_t pos) {
		input.rewind_to(pos);
		buffer.clear();
	}

	bool is_end_of_stmt();
	void take_end_of_stmt();
private:
	Token lex();

	Reader input;
	std::deque<Token> buffer;
	bool has_newline;
};

} // namespace rin
