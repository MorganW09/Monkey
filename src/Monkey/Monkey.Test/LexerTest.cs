using System;
using System.Collections.Generic;
using System.Text;
using Monkey;
using Xunit;

namespace Monkey.Test
{
    public class LexerTest
    {
        
        [Fact]
        public void Can_lex_symbols()
        {
            var input = "=+(){},;";

            //var tests = new List<(TokenType, string)>()
            //{
            //    new (TokenType., "="),
            //    (TokenType.PLUS, "+")
            //}
            ///TokenType.ASSIGN, "=")
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
                var tok = lexer.NextToken();

                Assert.Equal(expectedToken, tok);
            }
        }

        [Fact]
        public void Can_lex_simple_program()
        {
            var input = @"let five = 5;
let ten = 10;

let add = fn(x, y) {
    x + y;
};

let result = add(five, ten);";

            var expectedTokens = new List<Token>()
            {
                //five
                new Token(TokenType.LET, "let"),
                new Token(TokenType.IDENT, "five"),
                new Token(TokenType.ASSIGN, "="),
                new Token(TokenType.INT, "5"),
                new Token(TokenType.SEMICOLON, ";"),
                //ten
                new Token(TokenType.LET, "let"),
                new Token(TokenType.IDENT, "ten"),
                new Token(TokenType.ASSIGN, "="),
                new Token(TokenType.INT, "10"),
                new Token(TokenType.SEMICOLON, ";"),
                //add
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
                //result
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
                new Token(TokenType.EOF, ""),
            };

            var lexer = new Lexer(input);

            foreach (var expectedToken in expectedTokens)
            {
                var tok = lexer.NextToken();

                Assert.Equal(expectedToken, tok);
            }
        }

        [Theory]
        [InlineData('a')]
        [InlineData('g')]
        [InlineData('z')]
        [InlineData('A')]
        [InlineData('G')]
        [InlineData('Z')]
        [InlineData('_')]
        public void Test_IsLetter_LettersPass(char char_)
        {
            Assert.True(Lexer.isLetter(char_));
        }

        [Theory]
        [InlineData('!')]
        [InlineData('(')]
        [InlineData('*')]
        [InlineData('<')]
        [InlineData('~')]
        [InlineData('|')]
        public void Test_IsLetter_NonLettersFails(char char_)
        {
            Assert.False(Lexer.isLetter(char_));
        }

        [Theory]
        [InlineData("let five = 5;", "let")]
        [InlineData("five = 5;", "five")]
        public void Test_ReadIdentifier(string input, string expectedIdentifier)
        {
            var lexer = new Lexer(input);
            var identifier = lexer.readIdentifier();
            Assert.Equal(expectedIdentifier, identifier);
        }

        [Theory]
        [InlineData("10;", "10")]
        [InlineData("1024sss", "1024")]
        [InlineData("1000", "1000")]
        public void Test_ReadNumber(string input, string expectedNumber)
        {
            var lexer = new Lexer(input);
            var number = lexer.readNumber();
            Assert.Equal(expectedNumber, number);
        }

        [Theory]
        [InlineData("fn", TokenType.FUNCTION)]
        [InlineData("let", TokenType.LET)]
        [InlineData("five", TokenType.IDENT)]
        [InlineData("ten", TokenType.IDENT)]
        [InlineData("one", TokenType.IDENT)]
        public void Test_LookupIdent(string ident, TokenType expectedTokenType)
        {
            var tokenType = Lexer.LookupIdent(ident);
            Assert.Equal(expectedTokenType, tokenType);
        }

        [Theory]
        [InlineData('0')]
        [InlineData('1')]
        [InlineData('2')]
        [InlineData('3')]
        [InlineData('4')]
        [InlineData('5')]
        [InlineData('6')]
        [InlineData('7')]
        [InlineData('8')]
        [InlineData('9')]
        public void Test_IsDigit_DigitsPass(char char_)
        {
            Assert.True(Lexer.isDigit(char_));
        }

        [Theory]
        [InlineData('!')]
        [InlineData('(')]
        [InlineData('*')]
        [InlineData('<')]
        [InlineData('~')]
        [InlineData('|')]
        [InlineData('a')]
        [InlineData('g')]
        [InlineData('z')]
        [InlineData('A')]
        [InlineData('G')]
        [InlineData('Z')]
        [InlineData('_')]
        public void Test_IsDigit_NonDigitsFail(char char_)
        {
            Assert.False(Lexer.isDigit(char_));
        }

        [Theory]
        [InlineData("Hello", 2, 'l')]
        [InlineData("How many times", 4, 'm')]
        [InlineData("Monkey Language", 7, 'L')]
        [InlineData("Monkey Language", 80, '\0')]
        public void Test_ReadChar(string input, int howManyReads, char expectedChar)
        {
            var lexer = new Lexer(input);
            for(int i = 0; i < howManyReads; i++)
            {
                lexer.readChar();
            }

            Assert.Equal(expectedChar, lexer.ch);
        }
    }
}
