using System;
using System.Collections.Generic;
using System.Text;
using FluentAssertions;
using MonkeyLang.Lexical;
using Xunit;

namespace MonkeyLang.Test
{
    public class LexerTests
    {

        [Theory]
        [InlineData("let x = y;", 'l')]
        [InlineData("=+(){},;", '=')]
        [InlineData("fn", 'f')]
        public void Test_LexerInitializesCorrectly(string input, char firstChar)
        {
            var lexer = new Lexer(input);

            Assert.Equal(input, lexer.input);
            Assert.Equal(0, lexer.position);
            Assert.Equal(1, lexer.readPosition);
            Assert.Equal(firstChar, lexer.ch);
        }

        [Theory]
        [InlineData("let x = y;", 't', 2, 3)]
        [InlineData("=+(){},;", '(', 2, 3)]
        [InlineData("fn", '\0', 1, 2)]
        public void Test_ReadChar(string input, char expectedChar, int expectedPosition, int expectedReadPosition)
        {
            var lexer = new Lexer(input);

            lexer.readChar();
            lexer.readChar();

            lexer.ch.Should().Be(expectedChar);
            lexer.position.Should().Be(expectedPosition);
            lexer.readPosition.Should().Be(expectedReadPosition);
        }

        [Theory]
        [InlineData("let x = y;", ' ')]
        [InlineData("=+(){},;", ')')]
        [InlineData("fn", '\0')]
        public void Test_PeekChar(string input, char expectedPeek)
        {
            var lexer = new Lexer(input);

            lexer.readChar();
            lexer.readChar();

            var peekChar = lexer.peekChar();

            peekChar.Should().Be(expectedPeek);
        }

        [Theory]
        [InlineData('a', true)]
        [InlineData('m', true)]
        [InlineData('z', true)]
        [InlineData('A', true)]
        [InlineData('J', true)]
        [InlineData('Z', true)]
        [InlineData('_', true)]
        [InlineData('@', false)]
        [InlineData('[', false)]
        [InlineData('0', false)]
        [InlineData('2', false)]
        [InlineData('{', false)]
        public void Test_IsLetter(char ch, bool expected)
        {
            var isLetter = Lexer.isLetter(ch);

            isLetter.Should().Be(expected);
        }

        [Fact]
        public void Test_NextToken()
        {
            string input = "=+(){},;";

            var expectedTokens = new List<Token>()
            {
                new Token(TokenType.ASSIGN, "="),
                new Token(TokenType.PLUS, "+"),
                new Token(TokenType.LPAREN, "("),
                new Token(TokenType.RPAREN, ")"),
                new Token(TokenType.LBRACE, "{"),
                new Token(TokenType.RBRACE, "}"),
                new Token(TokenType.COMMA, ","),
                new Token(TokenType.SEMICOLON, ";"),
                new Token(TokenType.EOF, "")
            };

            var lexer = new Lexer(input);
            foreach (var expectedToken in expectedTokens)
            {
                var nextToken = lexer.NextToken();
                Assert.Equal(expectedToken.Type, nextToken.Type);
                Assert.Equal(expectedToken.Literal, nextToken.Literal);
            }
        }

        [Fact]
        public void Test_Small_Program()
        {
            string input = @"let five = 5;
let ten = 10;

let add = fn(x, y) {
    x + y;
};

let result = add(five, ten);
";
            var expectedTokens = new List<Token>()
            {
                new Token(TokenType.LET, "let"),
                new Token(TokenType.IDENT, "five"),
                new Token(TokenType.ASSIGN, "="),
                new Token(TokenType.INT, "5"),
                new Token(TokenType.SEMICOLON, ";"),

                new Token(TokenType.LET, "let"),
                new Token(TokenType.IDENT, "ten"),
                new Token(TokenType.ASSIGN, "="),
                new Token(TokenType.INT, "10"),
                new Token(TokenType.SEMICOLON, ";"),

                new Token(TokenType.LET, "let"),
                new Token(TokenType.IDENT, "add"),
                new Token(TokenType.ASSIGN, "="),
                new Token(TokenType.FUNCTION, "fn"),
                new Token(TokenType.LPAREN, "("),
                new Token(TokenType.IDENT, "x"),
                new Token(TokenType.COMMA, ","),
                new Token(TokenType.IDENT, "y"),
                new Token(TokenType.RPAREN, ")"),
                new Token(TokenType.LBRACE, "{"),
                new Token(TokenType.IDENT, "x"),
                new Token(TokenType.PLUS, "+"),
                new Token(TokenType.IDENT, "y"),
                new Token(TokenType.SEMICOLON, ";"),
                new Token(TokenType.RBRACE, "}"),
                new Token(TokenType.SEMICOLON, ";"),

                new Token(TokenType.LET, "let"),
                new Token(TokenType.IDENT, "result"),
                new Token(TokenType.ASSIGN, "="),
                new Token(TokenType.IDENT, "add"),
                new Token(TokenType.LPAREN, "("),
                new Token(TokenType.IDENT, "five"),
                new Token(TokenType.COMMA, ","),
                new Token(TokenType.IDENT, "ten"),
                new Token(TokenType.RPAREN, ")"),
                new Token(TokenType.SEMICOLON, ";"),
                new Token(TokenType.EOF, "")

            };

            var lexer = new Lexer(input);
            foreach (var expectedToken in expectedTokens)
            {
                var nextToken = lexer.NextToken();
                Assert.Equal(expectedToken.Type, nextToken.Type);
                Assert.Equal(expectedToken.Literal, nextToken.Literal);
            }
        }

        [Fact]
        public void Test_More_Operators()
        {
            var input = @"
!-/*5;
5 < 10 > 5;
";

            var expectedTokens = new List<Token>()
            {
                new Token(TokenType.BANG, "!"),
                new Token(TokenType.MINUS, "-"),
                new Token(TokenType.SLASH, "/"),
                new Token(TokenType.ASTERISK, "*"),
                new Token(TokenType.INT, "5"),
                new Token(TokenType.SEMICOLON, ";"),
                new Token(TokenType.INT, "5"),
                new Token(TokenType.LT, "<"),
                new Token(TokenType.INT, "10"),
                new Token(TokenType.GT, ">"),
                new Token(TokenType.INT, "5"),
                new Token(TokenType.SEMICOLON, ";"),
                new Token(TokenType.EOF, "")
            };

            var lexer = new Lexer(input);
            foreach (var expectedToken in expectedTokens)
            {
                var nextToken = lexer.NextToken();
                Assert.Equal(expectedToken.Type, nextToken.Type);
                Assert.Equal(expectedToken.Literal, nextToken.Literal);
            }
        }

        [Fact]
        public void Test_Conditional_Items()
        {
            var input = @"
if (5 < 10) {
    return true;
} else {
    return false;
}";

            var expectedTokens = new List<Token>()
            {
                new Token(TokenType.IF, "if"),
                new Token(TokenType.LPAREN, "("),
                new Token(TokenType.INT, "5"),
                new Token(TokenType.LT, "<"),
                new Token(TokenType.INT, "10"),
                new Token(TokenType.RPAREN, ")"),
                new Token(TokenType.LBRACE, "{"),
                new Token(TokenType.RETURN, "return"),
                new Token(TokenType.TRUE, "true"),
                new Token(TokenType.SEMICOLON, ";"),

                new Token(TokenType.RBRACE, "}"),
                new Token(TokenType.ELSE, "else"),
                new Token(TokenType.LBRACE, "{"),


                new Token(TokenType.RETURN, "return"),
                new Token(TokenType.FALSE, "false"),
                new Token(TokenType.SEMICOLON, ";"),

                new Token(TokenType.RBRACE, "}"),
                new Token(TokenType.EOF, "")
            };

            var lexer = new Lexer(input);
            foreach (var expectedToken in expectedTokens)
            {
                var nextToken = lexer.NextToken();
                Assert.Equal(expectedToken.Type, nextToken.Type);
                Assert.Equal(expectedToken.Literal, nextToken.Literal);
            }
        }

        [Fact]
        public void Test_Double_Comparisons()
        {
            var input = @"
10 == 10;
10 != 9;
";
            var expectedTokens = new List<Token>()
            {
                new Token(TokenType.INT, "10"),
                new Token(TokenType.EQ, "=="),
                new Token(TokenType.INT, "10"),
                new Token(TokenType.SEMICOLON, ";"),

                new Token(TokenType.INT, "10"),
                new Token(TokenType.NOT_EQ, "!="),
                new Token(TokenType.INT, "9"),
                new Token(TokenType.SEMICOLON, ";"),
                new Token(TokenType.EOF, "")
            };

            var lexer = new Lexer(input);
            foreach (var expectedToken in expectedTokens)
            {
                var nextToken = lexer.NextToken();
                Assert.Equal(expectedToken.Type, nextToken.Type);
                Assert.Equal(expectedToken.Literal, nextToken.Literal);
            }

        }
    }
}
