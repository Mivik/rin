
#include <vector>

#include <gtest/gtest.h>

#include "lexer.h"

namespace rin {

inline std::pair<Lexer, MemoryBuffer> lex_pair(const char *str) {
	MemoryBuffer buffer(str);
	Lexer lexer(buffer);
	return { lexer, buffer };
}

inline std::vector<Token> tokens(Lexer &lexer) {
	std::vector<Token> ret;
	while (Token token = lexer.take())
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
	"const", "else", "enum", "fn", "for", "if",
	"in", "is", "let", "return", "var", "when"
};

TEST(lexer, identifier) {
	constexpr const char *TESTCASES[] = {
		"hello", "world", "x1", "x2", "sum_of_prices",
		"_var", "$1", "$_$", "_$1"
	};
	for (auto str : TESTCASES)
		EXPECT_TRUE(tokens_eq(str, { Identifier }));
	EXPECT_FALSE(tokens_eq("1and2", { Identifier }));
	for (auto keyword : KEYWORDS)
		EXPECT_FALSE(tokens_eq(keyword, { Identifier }));
}

TEST(lexer, string) {
	constexpr const char *TESTCASES[] = {
		"\"string\"", R"("\"I'm on the block!\" the man said")",
		"\"200 OK\n\""
	};
	for (auto str : TESTCASES)
		EXPECT_TRUE(tokens_eq(str, { String }));
	EXPECT_THROW(tokens("\"unterminated"), LexException);
}

TEST(lexer, operator) {
	EXPECT_TRUE(tokens_eq(
		(
			"+ - * / % << >> | & ~ ^ || && ! "
			"= += -= *= /= %= <<= >>= |= &= ^= "
			"< > <= >= == !="
		),
		{
			Add, Sub, Mul, Div, Mod, Shl, Shr, Or, And, Not, Xor, LOr, LAnd, LNot,
			Assign, AddA, SubA, MulA, DivA, ModA, ShlA, ShrA, OrA, AndA, XorA,
			Lt, Gt, Le, Ge, Eq, Neq
		}
	));
}

TEST(lexer, delimiter) {
	EXPECT_TRUE(tokens_eq(
		"() [] {} : , . ->",
		{
			LPar, RPar, LBracket, RBracket, LBrace, RBrace,
			Colon, Comma, Period, Arrow
		}
	));
}

TEST(lexer, comment) {
	EXPECT_TRUE(tokens_eq(
		(
			"// Mivik 2020.12.11\n"
			"// This program calculates (a / b) /*wow*/\n"
			"/**\n"
			"* @param a a number\n"
			"* @param b a number\n"
			"*/"
		),
		{ Comment, Comment, MLComment }
	));
	EXPECT_THROW(tokens("/*unterminated"), LexException);
	EXPECT_TRUE(tokens_eq("/* a * b */", { MLComment }));
}

TEST(lexer, adjacent) {
	EXPECT_TRUE(tokens_eq(
		"1>2 && 2<3",
		{ Number, Gt, Number, LAnd, Number, Lt, Number }
	));
	EXPECT_TRUE(tokens_eq(
		"(a_!=b_)",
		{ LPar, Identifier, Neq, Identifier, RPar }
	));
}

TEST(lexer, integer) {
	EXPECT_TRUE(tokens_eq(
		"0123", // 0 is a single number itself
		{ Number, Number }
	));
	EXPECT_TRUE(tokens_eq(
		"19260817 1ll 2ull 3u",
		{ Number, Number, Number, Number }
	));
	EXPECT_THROW(tokens("1l"), LexException);
	EXPECT_THROW(tokens("1ule"), LexException);
}

TEST(lexer, boolean) {
	EXPECT_TRUE(tokens_eq(
		"true false",
		{ True, False }
	));
}

TEST(lexer, special_char) {
	EXPECT_THROW(tokens("`"), LexException);
	EXPECT_THROW(tokens("春"), LexException);
	EXPECT_TRUE(tokens_eq("\"春\"", { String }));
}

} // namespace rin
