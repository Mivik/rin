
#include <cctype>

#include "lexer.h"
#include "token.h"

namespace rin {

std::string MemoryBuffer::substr(const SourceRange &range) const {
	return std::string(begin + range.begin, begin + range.end);
}

void Reader::rewind(size_t count) {
	if (ptr - count < buffer.begin) throw LexException("Illegal rewind range!");
	ptr -= count;
}

inline constexpr size_t string_hash(const char *str) {
	size_t ret = 0;
	while (*str) ret = ret * 31 + *str++;
	return ret;
}

inline TokenKind word_kind(const std::string &str) {
#define CASE(s, k) case string_hash(s): if (str == s) return k; break;
	switch (string_hash(str.data())) {
		CASE("const", Const)
		CASE("else", Else)
		CASE("enum", Enum)
		CASE("fn", Fn)
		CASE("for", For)
		CASE("if", If)
		CASE("in", In)
		CASE("is", Is)
		CASE("let", Let)
		CASE("return", Return)
		CASE("var", Var)
		CASE("when", When)
		CASE("true", True)
		CASE("false", False)
	}
#undef CASE
	return Identifier;
}

Token Lexer::lex() {
	while (isspace(input.peek())) input.take();
	if (input.peek() == -1) return Token(Eof, SourceRange::empty());
	const size_t begin = input.position();
	auto range = [&] { return SourceRange(begin, input.position()); };
	auto token = [&](TokenKind kind) { return Token(kind, range()); };
	auto throw_with_msg = [&](const std::string &msg) {
		throw LexException(msg + ": " + get_buffer().substr(range()));
	};
	switch (const char st = input.take()) {
		case 'a' ... 'z':
		case 'A' ... 'Z':
		case '$':
		case '_': {
			while (true) {
				const char c = input.peek();
				if (isdigit(c) || isalpha(c) || c == '$' || c == '_') input.take();
				else break;
			}
			return token(word_kind(get_buffer().substr(range())));
		}
		case '0':
			return token(Number); // TODO: octal support
		case '1' ... '9': { // currently integer only
			while (true) {
				if (isdigit(input.peek())) input.take();
				else break;
			}
			auto illegal_suffix = [&]() {
				throw_with_msg("Integer with illegal suffix");
			};
			switch (tolower(input.take())) {
				case 'u': {
					if (tolower(input.peek()) == 'l') {
						input.take();
						if (tolower(input.take()) != 'l') illegal_suffix();
					}
					break;
				}
				case 'l': {
					if (tolower(input.take()) != 'l') illegal_suffix();
					break;
				}
				default:
					input.rewind();
					break;
			}
			return token(Number);
		}
		case '\'':
		case '"': {
			while (true) {
				switch (const char c = input.take()) {
					case -1:
						throw_with_msg("Unterminated string");
					case '\\':
						input.take();
						break;
					default:
						if (st == c) return token(String);
						break;
				}
			}
		}
#define bop(op, kind) \
    case op: { \
        if (input.peek() == '=') { \
            input.take(); \
            return token(kind##A); \
        } \
        return token(kind); \
    }
		bop('+', Add)
		bop('*', Mul)
		bop('%', Mod)
		bop('^', Xor)
#undef bop

		case '-': {
			if (input.peek() == '=') {
				input.take();
				return token(SubA);
			}
			if (input.peek() == '>') {
				input.take();
				return token(Arrow);
			}
			return token(Sub);
		}
		case '~':
			return token(Not);
		case '<':
		case '>': {
			const bool right = st == '>';
			const char c = input.take();
			if (c == st) {
				if (input.peek() == '=') {
					input.take();
					return token(right? ShrA: ShlA);
				}
				return token(right? Shr: Shl);
			} else if (c == '=') return token(right? Ge: Le);
			else {
				input.rewind();
				return token(right? Gt: Lt);
			}
		}
		case '=': {
			if (input.peek() == '=') {
				input.take();
				return token(Eq);
			}
			return token(Assign);
		}
		case '|':
		case '&': {
			const bool ad = st == '&';
			const char c = input.take();
			if (c == st) return token(ad? LAnd: LOr);
			else if (c == '=') return token(ad? AndA: OrA);
			else {
				input.rewind();
				return token(ad? And: Or);
			}
		}
		case '!': {
			if (input.peek() == '=') {
				input.take();
				return token(Neq);
			}
			return token(LNot);
		}
		case '/': {
			switch (input.take()) {
				case '=':
					return token(DivA);
				case '/': {
					while (true) {
						const char c = input.take();
						if (c == -1 || c == '\n') break;
					}
					input.rewind();
					return token(Comment);
				}
				case '*': {
					auto illegal = [&]() {
						throw_with_msg("Unterminated multiline comment");
					};
					while (true) {
						const char c = input.take();
						if (c == -1) illegal();
						if (c == '*') {
							const char nxt = input.take();
							if (nxt == '/') break;
							if (nxt == -1) illegal();
							input.rewind();
						}
					}
					return token(MLComment);
				}
				default:
					input.rewind();
					return token(Div);
			}
		}
		case '(':
			return token(LPar);
		case ')':
			return token(RPar);
		case '[':
			return token(LBracket);
		case ']':
			return token(RBracket);
		case '{':
			return token(LBrace);
		case '}':
			return token(RBrace);
		case ':':
			return token(Colon);
		case ';':
			return token(Semicolon);
		case ',':
			return token(Comma);
		case '.':
			return token(Period);
		default:
			throw LexException((std::string) "Unknown char " + st);
	}
}

} // namespace rin
