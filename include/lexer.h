
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
		input(buffer) {}
	[[nodiscard]] const MemoryBuffer &get_buffer() const { return input.get_buffer(); }
	inline Token peek() {
		if (buffer.empty()) buffer.push_back(lex());
		return buffer.front();
	}
	inline Token take() {
		if (buffer.empty()) return lex();
		Token ret = buffer.front();
		buffer.pop_front();
		return ret;
	}
	inline size_t position() {
		if (buffer.empty()) return input.position();
		return buffer.front().range.begin;
	}
	inline void rewind_to(size_t pos) {
		input.rewind_to(pos);
		buffer.clear();
	}
private:
	Token lex();

	Reader input;
	std::deque<Token> buffer;
};

} // namespace rin
