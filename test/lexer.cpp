
#include <vector>

#include <gtest/gtest.h>

#include "lexer.h"

namespace rin {

inline std::pair<Lexer, std::string> lex_pair(std::string str) {
	std::pair<Lexer, std::string> res{ "", std::move(str) };
	res.first = Lexer(res.second);
	return res;
}

inline std::vector<Token> tokens(Lexer &lexer) {
	std::vector<Token> ret;
	while (Token token = lexer.take(false))
		ret.push_back(token);
	return ret;
}

inline std::vector<Token> tokens(const char *str) {
	auto[lexer, _] = lex_pair(str);
	return tokens(lexer);
}

::testing::AssertionResult tokens_eq(
	const char *content,
	const std::vector<TokenKind> &expected
) {
	auto[lexer, buffer] = lex_pair(content);
	auto value = tokens(lexer);
	if (value.size() != expected.size())
		return ::testing::AssertionFailure()
			<< "Token list size differs. "
			<< "Expected " << expected.size() << " tokens, "
			<< "got " << value.size();
	for (size_t i = 0; i < value.size(); ++i)
		if (value[i].kind != expected[i])
			return ::testing::AssertionFailure()
				<< "No. " << (i + 1) << " token differs. "
				<< "Expected " << token_kind::name(expected[i]) << ", "
				<< "got " << value[i].info(buffer);
	return ::testing::AssertionSuccess();
}

inline constexpr const char *KEYWORDS[] = {
	"do", "else", "enum", "false", "true", "fn", "for",
	"if", "inline", "mut", "return", "struct",
	"true", "var", "val", "when", "while"
};

#define K TokenKind

TEST(lexer, identifier) {
	constexpr const char *TESTCASES[] = {
		"hello", "world", "x1", "x2", "sum_of_prices",
		"_var", "$1", "$_$", "_$1"
	};
	for (auto str : TESTCASES)
		EXPECT_TRUE(tokens_eq(str, { K::Identifier }));
	EXPECT_FALSE(tokens_eq("1and2", { K::Identifier }));
	for (auto keyword : KEYWORDS)
		EXPECT_FALSE(tokens_eq(keyword, { K::Identifier }));
}

TEST(lexer, string) {
	constexpr const char *TESTCASES[] = {
		"\"string\"", R"("\"I'm on the block!\" the man said")",
		"\"200 OK\n\""
	};
	for (auto str : TESTCASES)
		EXPECT_TRUE(tokens_eq(str, { K::String }));
	EXPECT_THROW(tokens("\"unterminated"), LexException);
}

TEST(lexer, operator) {
	std::vector<TokenKind> expected = {
#define TOKEN_BINARY_OP(name) K::name,

#include "token.def"

		K::Not, K::LNot
	};
	EXPECT_TRUE(tokens_eq(
		(
			"+ += & &= = / /= == >= > "
			"&& [ || <= < % %= * *= != "
			"| |= . << <<= >> >>= - -= "
			"^ ^= ~ !"
		),
		expected
	));
}

TEST(lexer, delimiter) {
	EXPECT_TRUE(tokens_eq(
		"() [] {} : , . ->",
		{
			K::LPar, K::RPar, K::LBracket, K::RBracket, K::LBrace, K::RBrace,
			K::Colon, K::Comma, K::Period, K::Arrow
		}
	));
}

TEST(lexer, comment) {
	EXPECT_TRUE(tokens_eq(
		(
			"// Mivik 2021.8.6\n"
			"// This program calculates (a / b) /*wow*/\n"
			"/**\n"
			"* @param a a number\n"
			"* @param b a number\n"
			"*/"
		),
		{ K::Comment, K::Comment, K::MLComment }
	));
	EXPECT_THROW(tokens("/*unterminated"), LexException);
	EXPECT_TRUE(tokens_eq("/* a * b */", { K::MLComment }));
}

TEST(lexer, adjacent) {
	EXPECT_TRUE(tokens_eq(
		"1>2 && 2<3",
		{ K::Number, K::Gt, K::Number, K::LAnd, K::Number, K::Lt, K::Number }
	));
	EXPECT_TRUE(tokens_eq(
		"(a_!=b_)",
		{ K::LPar, K::Identifier, K::Neq, K::Identifier, K::RPar }
	));
}

TEST(lexer, integer) {
	EXPECT_TRUE(tokens_eq(
		"0123", // TODO 0 is a single number itself since we don't support octal now
		{ K::Number, K::Number }
	));
	EXPECT_TRUE(tokens_eq(
		"19260817 1ll 2ull 3u",
		{ K::Number, K::Number, K::Number, K::Number }
	));
	EXPECT_THROW(tokens("1ule"), LexException);
}

TEST(lexer, boolean) {
	EXPECT_TRUE(tokens_eq(
		"true false",
		{ K::True, K::False }
	));
}

TEST(lexer, special_char) {
	EXPECT_THROW(tokens("`"), LexException);
	EXPECT_THROW(tokens("春"), LexException);
	EXPECT_TRUE(tokens_eq("\"春\"", { K::String }));
}

} // namespace rin
