
#include "lexer.h"

namespace rin {

inline constexpr size_t string_hash(std::string_view str) {
	size_t res = 0;
	for (char c : str) res = res * 31 + c;
	return res;
}

inline TokenKind word_kind(std::string_view str) {
	if (str[0] == '@') return TokenKind::Builtin;
#define CASE(s, k) case string_hash(s): if (str == s) return TokenKind::k; break;
	switch (string_hash(str)) {
		CASE("const", Const)
		CASE("do", Do)
		CASE("else", Else)
		CASE("enum", Enum)
		CASE("false", False)
		CASE("fn", Fn)
		CASE("for", For)
		CASE("if", If)
		CASE("inline", Inline)
		CASE("return", Return)
		CASE("struct", Struct)
		CASE("true", True)
		CASE("val", Val)
		CASE("var", Var)
		CASE("when", When)
		CASE("while", While)
	}
#undef CASE
	return TokenKind::Identifier;
}

Token Lexer::peek(bool ignore_comment) {
	if (ignore_comment) {
		while (true) {
			auto token = peek(false);
			if (token.kind == TokenKind::Comment || token.kind == TokenKind::MLComment)
				take(false);
			else return token;
		}
	}
	if (buffer.empty()) buffer.push_back(lex());
	return buffer.front();
}

Token Lexer::take(bool ignore_comment) {
	if (ignore_comment) {
		while (true) {
			auto token = take(false);
			if (token.kind == TokenKind::Comment || token.kind == TokenKind::MLComment) continue;
			else return token;
		}
	}
	if (buffer.empty()) return lex();
	auto res = buffer.front();
	buffer.pop_front();
	return res;
}

Token Lexer::lex() {
	while (!input.eof() && isspace(input.peek())) input.take();
	if (input.eof()) return Token(TokenKind::Eof, SourceRange::make_empty());
	const size_t begin = input.position();
	auto range = [&]() { return SourceRange(begin, input.position()); };
	auto token = [&](TokenKind kind) { return Token(kind, range()); };
	auto word = [&]() { return input.substr(range()); };
	auto error = [&](const std::string &msg) {
		throw LexException(msg + ": " + std::string(word()));
	};

	switch (char st = input.take()) {
		case 'a' ... 'z':
		case 'A' ... 'Z':
		case '$':
		case '@':
		case '_': {
			while (!input.eof()) {
				const char c = input.peek();
				if (isdigit(c) || isalpha(c) || c == '$' || c == '_') input.take();
				else break;
			}
			return token(word_kind(input.substr(range())));
		}
		case '0':
			return token(TokenKind::Number); // TODO octal support
		case '1' ... '9': { // currently integer only
			while (!input.eof() && isdigit(input.peek())) input.take();
			const char *illegal_suffix = "Integer with illegal suffix";
			if (!input.eof())
				switch (tolower(input.take())) {
					case 'u': {
						if (!input.eof() && tolower(input.peek()) == 'l') {
							input.take();
							if (tolower(input.take()) != 'l') error(illegal_suffix);
						}
						break;
					}
					case 'l': {
						if (!input.eof() && tolower(input.peek()) == 'l') input.take();
						break;
					}
					default:
						input.rewind();
						break;
				}
			return token(TokenKind::Number);
		}
		case '\'':
		case '"': {
			const char *unterminated = "Unterminated string";
			while (true) {
				if (input.eof()) error(unterminated);
				const char c = input.take();
				if (c == '\\') {
					if (input.eof()) error(unterminated);
					input.take();
				} else if (st == c) return token(TokenKind::String);
			}
		}
#define bop(op, kind) \
    case op: { \
        if (!input.eof() && input.peek() == '=') { \
            input.take(); \
            return token(TokenKind::kind##A); \
        } \
        return token(TokenKind::kind); \
    }
		bop('+', Add)
		bop('*', Mul)
		bop('%', Mod)
		bop('^', Xor)
#undef bop

		case '-': {
			if (!input.eof()) {
				if (input.peek() == '=') {
					input.take();
					return token(TokenKind::SubA);
				}
				if (input.peek() == '>') {
					input.take();
					return token(TokenKind::Arrow);
				}
			}
			return token(TokenKind::Sub);
		}
		case '~':
			return token(TokenKind::Not);
		case '<':
		case '>': {
			const bool right = st == '>';
			const char c = input.take();
			if (c == st) {
				if (!input.eof() && input.peek() == '=') {
					input.take();
					return token(right? TokenKind::ShrA: TokenKind::ShlA);
				}
				return token(right? TokenKind::Shr: TokenKind::Shl);
			} else if (c == '=') return token(right? TokenKind::Ge: TokenKind::Le);
			else {
				input.rewind();
				return token(right? TokenKind::Gt: TokenKind::Lt);
			}
		}
		case '=': {
			if (!input.eof() && input.peek() == '=') {
				input.take();
				return token(TokenKind::Eq);
			}
			return token(TokenKind::Assign);
		}
		case '|':
		case '&': {
			const bool ad = st == '&';
			const char c = input.take();
			if (c == st) return token(ad? TokenKind::LAnd: TokenKind::LOr);
			else if (c == '=') return token(ad? TokenKind::AndA: TokenKind::OrA);
			else {
				input.rewind();
				return token(ad? TokenKind::And: TokenKind::Or);
			}
		}
		case '!': {
			if (!input.eof() && input.peek() == '=') {
				input.take();
				return token(TokenKind::Neq);
			}
			return token(TokenKind::LNot);
		}
		case '/': {
			if (!input.eof())
				switch (input.take()) {
					case '=':
						return token(TokenKind::DivA);
					case '/': {
						while (!input.eof() && input.take() != '\n');
						input.rewind();
						return token(TokenKind::Comment);
					}
					case '*': {
						const char *unterminated = "Unterminated multiline comment";
						while (true) {
							if (input.eof()) error(unterminated);
							if (input.take() == '*') {
								if (input.eof()) error(unterminated);
								if (input.take() == '/') break;
								input.rewind();
							}
						}
						return token(TokenKind::MLComment);
					}
					default:
						break;
				}
			input.rewind();
			return token(TokenKind::Div);
		}
		case '(':
			return token(TokenKind::LPar);
		case ')':
			return token(TokenKind::RPar);
		case '[':
			return token(TokenKind::LBracket);
		case ']':
			return token(TokenKind::RBracket);
		case '{':
			return token(TokenKind::LBrace);
		case '}':
			return token(TokenKind::RBrace);
		case ':':
			return token(TokenKind::Colon);
		case ';':
			return token(TokenKind::Semicolon);
		case ',':
			return token(TokenKind::Comma);
		case '.':
			return token(TokenKind::Period);
		default:
			throw LexException(std::string("Unknown char ") + st);
	}
}

} // namespace rin
