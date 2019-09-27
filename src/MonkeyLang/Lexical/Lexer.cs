using System;
using System.Collections.Generic;
using System.Text;

namespace MonkeyLang.Lexical
{
    public class Lexer
    {
        public string input { get; }
        public int position { get; set; }
        public int readPosition { get; set; }
        public char ch { get; set; }

        Dictionary<string, string> keywords = new Dictionary<string, string>()
        {
            { "fn", TokenType.FUNCTION },
            { "let", TokenType.LET },
            { "true", TokenType.TRUE },
            { "false", TokenType.FALSE },
            { "if", TokenType.IF },
            { "else", TokenType.ELSE },
            { "return", TokenType.RETURN }
        };

        public Lexer(string input)
        {
            this.input = input;
            readChar();
        }

        public void readChar()
        {
            if (readPosition >= input.Length)
            {
                ch = '\0';
            }
            else
            {
                ch = input[readPosition];
            }

            position = readPosition;
            readPosition++;
        }

        public char peekChar()
        {
            if (readPosition >= input.Length)
            {
                return '\0';
            }
            else
            {
                return input[readPosition];
            }
        }

        internal bool isLetter(char @char)
        {
            return 'a' <= @char && @char <= 'z'
                || 'A' <= @char && @char <= 'Z'
                || '_' == @char;
        }

        internal bool isDigit(char @char)
        {
            return '0' <= @char && @char <= '9';
        }

        internal string lookupIdent(string ident)
        {
            if (keywords.TryGetValue(ident, out string keyword))
            {
                return keyword;
            }
            return TokenType.IDENT;
        }

        internal string readIdentifier()
        {
            var pos = position;
            while (isLetter(ch))
            {
                readChar();
            }
            return input.Substring(pos, position - pos);
        }

        internal string readNumber()
        {
            var pos = position;
            while (isDigit(ch))
            {
                readChar();
            }
            return input.Substring(pos, position - pos);
        }

        internal void skipWhitespace()
        {
            while (ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r')
            {
                readChar();
            }
        }

        public Token NextToken()
        {
            skipWhitespace();

            Token t;
            switch (ch)
            {
                case '=':
                    if (peekChar() == '=')
                    {
                        var @char = ch;
                        readChar();
                        t = new Token(TokenType.EQ, @char.ToString() + ch.ToString());
                    }
                    else
                    {
                        t = new Token(TokenType.ASSIGN, ch.ToString());
                    }
                    break;
                case '!':
                    if (peekChar() == '=')
                    {
                        var @char = ch;
                        readChar();
                        t = new Token(TokenType.NOT_EQ, @char.ToString() + ch.ToString());
                    }
                    else
                    {
                        t = new Token(TokenType.BANG, ch.ToString());
                    }
                    break;
                case ';':
                    t = new Token(TokenType.SEMICOLON, ch.ToString());
                    break;
                case '(':
                    t = new Token(TokenType.LPAREN, ch.ToString());
                    break;
                case ')':
                    t = new Token(TokenType.RPAREN, ch.ToString());
                    break;
                case ',':
                    t = new Token(TokenType.COMMA, ch.ToString());
                    break;
                case '+':
                    t = new Token(TokenType.PLUS, ch.ToString());
                    break;
                case '{':
                    t = new Token(TokenType.LBRACE, ch.ToString());
                    break;
                case '}':
                    t = new Token(TokenType.RBRACE, ch.ToString());
                    break;
                case '-':
                    t = new Token(TokenType.MINUS, ch.ToString());
                    break;
                case '/':
                    t = new Token(TokenType.SLASH, ch.ToString());
                    break;
                case '*':
                    t = new Token(TokenType.ASTERISK, ch.ToString());
                    break;
                case '<':
                    t = new Token(TokenType.LT, ch.ToString());
                    break;
                case '>':
                    t = new Token(TokenType.GT, ch.ToString());
                    break;
                case '\0':
                    t = new Token(TokenType.EOF, "");
                    break;
                default:
                    if (isLetter(ch))
                    {
                        var literal = readIdentifier();
                        var type = lookupIdent(literal);
                        return new Token(type, literal);
                    }
                    else if (isDigit(ch))
                    {
                        var literal = readNumber();
                        var type = TokenType.INT;
                        return new Token(type, literal);
                    }
                    else
                    {
                        t = new Token(TokenType.ILLEGAL, ch.ToString());
                    }
                    break;
            }

            readChar();
            return t!;
        }
    }
}
