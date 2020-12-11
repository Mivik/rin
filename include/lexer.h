
#pragma once

#include <cstring>
#include <exception>
#include <memory>
#include <vector>

#include "token.h"

namespace rin {

class SourceRange;
class Token;

class LexException : public std::exception {
public:
	const char *what() const noexcept { return msg.data(); }
private:
	std::string msg;
	LexException(const std::string &msg): msg(msg) {}

	friend class Lexer;
	friend class Reader;
};

class MemoryBuffer {
public:
	const char * const begin, * const end;

	MemoryBuffer(const char *begin, const char *end):
		begin(begin), end(end) {}
	explicit MemoryBuffer(const char *str):
		begin(str), end(str + strlen(str)) {}

	std::string substr(const SourceRange &range) const;
};

class Reader {
public:
	Reader(const MemoryBuffer &buffer):
		buffer(buffer), ptr(buffer.begin) {}

	const MemoryBuffer& get_buffer() const { return buffer; }
	size_t position() const { return ptr - buffer.begin; }
	char peek() const { return (ptr >= buffer.end)? -1: *ptr; }
	char take() { const char r = peek(); ++ptr; return r; }
	void rewind(size_t count = 1);
private:
	MemoryBuffer buffer;
	const char *ptr;
};

class Lexer {
public:
	Lexer(MemoryBuffer buffer):
		input(buffer) {}
	const MemoryBuffer& get_buffer() const { return input.get_buffer(); }
	Token lex();
private:
	Reader input;
};

} // namespace rin
