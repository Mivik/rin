
#pragma once

#include <cassert>
#include <cstdio>
#include <deque>
#include <string_view>

#include "token.h"

namespace rin {

class LexException : public std::exception {
public:
	[[nodiscard]] const char *what() const noexcept override { return msg.data(); }

	explicit LexException(std::string msg): msg(std::move(msg)) {}
private:
	std::string msg;
};

class Reader {
public:
	explicit Reader(std::string_view buffer):
		buffer(buffer), ptr(buffer.data()) {}

	[[nodiscard]] std::string_view get_buffer() const { return buffer; }
	[[nodiscard]] std::string_view substr(SourceRange range) const {
		return buffer.substr(range.begin, range.end - range.begin);
	}
	[[nodiscard]] bool eof() const { return ptr == buffer.data() + buffer.size(); }
	[[nodiscard]] size_t position() const { return ptr - buffer.data(); }
	[[nodiscard]] char peek() const {
		if (eof())
			throw LexException("Unexpected EOF");
		return *ptr;
	}
	void rewind_to(size_t pos) { ptr = buffer.data() + pos; }
	char take() {
		char r = peek();
		++ptr;
		return r;
	}
	void rewind(size_t count = 1) {
		if (ptr - count < buffer.data()) throw LexException("Illegal rewind range!");
		ptr -= count;
	}

private:
	std::string_view buffer;
	const char *ptr;
};

class Lexer {
public:
	explicit Lexer(std::string_view buffer):
		input(buffer) {}

	[[nodiscard]] std::string_view get_buffer() const { return input.get_buffer(); }
	[[nodiscard]] const Reader &get_reader() const { return input; }
	size_t position() {
		if (buffer.empty()) return input.position();
		return buffer.front().range.begin;
	}
	void rewind_to(size_t pos) {
		input.rewind_to(pos);
		buffer.clear();
	}

	Token peek(bool ignore_comment = true);
	Token take(bool ignore_comment = true);

private:
	Token lex();

	Reader input;
	std::deque<Token> buffer;
};

} // namespace rin
