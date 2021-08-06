
#include "lexer.h"

namespace rin {

inline constexpr size_t string_hash(std::string_view str) {
	size_t ret = 0;
	for (char c : str) ret = ret * 31 + c;
	return ret;
}

inline TokenKind word_kind(std::string_view str) {
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
		CASE("return", Return)
		CASE("true", True)
		CASE("var", Var)
		CASE("val", Val)
		CASE("when", When)
		CASE("while", While)
		default:
			return TokenKind::Identifier;
	}
#undef CASE
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
	auto ret = buffer.front();
	buffer.pop_front();
	return ret;
}

Token Lexer::lex() {
	while (isspace(input.peek())) input.take();
	if (input.peek() == EOF) return Token(TokenKind::Eof, SourceRange::make_empty());
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
		case '_': {
			while (true) {
				char c = input.peek();
				if (isdigit(c) || isalpha(c) || c == '$' || c == '_') input.take();
				else break;
			}
			return token(word_kind(input.substr(range())));
		}
		case '0':
			return token(TokenKind::Number); // TODO octal support
		case '1' ... '9': { // currently integer only
			while (true) {
				if (isdigit(input.peek())) input.take();
				else break;
			}
			const char *illegal_suffix = "Integer with illegal suffix";
			switch (tolower(input.take())) {
				case 'u': {
					if (tolower(input.peek()) == 'l') {
						input.take();
						if (tolower(input.take()) != 'l') error(illegal_suffix);
					}
					break;
				}
				case 'l': {
					if (tolower(input.take()) != 'l') error(illegal_suffix);
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
			while (true) {
				switch (const char c = input.take()) {
					case -1:
						error("Unterminated string");
					case '\\':
						input.take();
						break;
					default:
						if (st == c) return token(TokenKind::String);
						break;
				}
			}
		}
#define bop(op, kind) \
    case op: { \
        if (input.peek() == '=') { \
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
			if (input.peek() == '=') {
				input.take();
				return token(TokenKind::SubA);
			}
			if (input.peek() == '>') {
				input.take();
				return token(TokenKind::Arrow);
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
				if (input.peek() == '=') {
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
			if (input.peek() == '=') {
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
			if (input.peek() == '=') {
				input.take();
				return token(TokenKind::Neq);
			}
			return token(TokenKind::LNot);
		}
		case '/': {
			switch (input.take()) {
				case '=':
					return token(TokenKind::DivA);
				case '/': {
					while (true) {
						const char c = input.take();
						if (c == -1 || c == '\n') break;
					}
					input.rewind();
					return token(TokenKind::Comment);
				}
				case '*': {
					const char *unterminated = "Unterminated multiline comment";
					while (true) {
						const char c = input.take();
						if (c == -1) error(unterminated);
						if (c == '*') {
							const char nxt = input.take();
							if (nxt == '/') break;
							if (nxt == -1) error(unterminated);
							input.rewind();
						}
					}
					return token(TokenKind::MLComment);
				}
				default:
					input.rewind();
					return token(TokenKind::Div);
			}
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
