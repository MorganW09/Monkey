using System;
using System.Collections.Generic;
using System.Text;

namespace Monkey
{
    public class Lexer
    {
        public string input { get; private set; }
        public int position { get; private set; }
        public int readPosition { get; private set; }
        public char ch { get; private set; }

        private static Dictionary<string, TokenType> keywords = new Dictionary<string, TokenType>()
        {
            { "fn", TokenType.FUNCTION },
            { "let", TokenType.LET }
        };

        public Lexer(string input)
        {
            this.input = input;
            readChar();
        }

        public void readChar()
        {
            if (readPosition >= input.Length) {
                ch = '\0';
            }
            else
            {
                ch = input[readPosition];
            }
            position = readPosition;
            readPosition++;
        }

        public Token NextToken()
        {

            skipWhitespace();

            Token tok = null;
            switch (ch) {
                case '=':
                    tok = new Token(TokenType.ASSIGN, ch.ToString());
                    break;
                case ';':
                    tok = new Token(TokenType.SEMICOLON, ch.ToString());
                    break;
                case '(':
                    tok = new Token(TokenType.LPAREN, ch.ToString());
                    break;
                case ')':
                    tok = new Token(TokenType.RPAREN, ch.ToString());
                    break;
                case ',':
                    tok = new Token(TokenType.COMMA, ch.ToString());
                    break;
                case '+':
                    tok = new Token(TokenType.PLUS, ch.ToString());
                    break;
                case '{':
                    tok = new Token(TokenType.LBRACE, ch.ToString());
                    break;
                case '}':
                    tok = new Token(TokenType.RBRACE, ch.ToString());
                    break;
                case '\0':
                    tok = new Token(TokenType.EOF, "");
                    break;
                default:
                    if (isLetter(ch))
                    {
                        var literal = readIdentifier();
                        var type = LookupIdent(literal);
                        tok = new Token(type, literal);
                        return tok;
                    }
                    else if (isDigit(ch))
                    {
                        var digit = readNumber();
                        tok = new Token(TokenType.INT, digit);
                        return tok;
                    }
                    else
                    {
                        tok = new Token(TokenType.ILLEGAL, ch.ToString());
                    }
                    break;
            }
            readChar();
            return tok;
        }

        public string readIdentifier()
        {
            var pos = position;
            while (isLetter(ch)) 
            {
                readChar();
            }
            return input.Substring(pos, (position - pos));
        }

        public string readNumber()
        {
            var pos = position;
            while (isDigit(ch))
            {
                readChar();
            }
            return input.Substring(pos, (position - pos));
        }

        public void skipWhitespace()
        {
            while (ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r')
            {
                readChar();
            }
        }

        public static TokenType LookupIdent(string ident)
        {
            if (keywords.TryGetValue(ident, out TokenType value))
            {
                return value;
            }
            return TokenType.IDENT;
        }

        public static bool isLetter(char char_)
        {
            var lowerCase = char_.CompareTo('a') >= 0 && char_.CompareTo('z') <= 0;
            var upperCase = char_.CompareTo('A') >= 0 && char_.CompareTo('Z') <= 0;
            var underscore = '_' == char_;
            return lowerCase || upperCase || underscore;
        }

        public static bool isDigit(char char_)
        {
            return char_.CompareTo('0') >= 0 && char_.CompareTo('9') <= 0;
        }
    }
}
