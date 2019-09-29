using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace MonkeyLang
{
    public class Token
    {
        public static readonly Dictionary<TokenType, string> tokenMap = new Dictionary<TokenType, string>
        {
            {TokenType.ILLEGAL, "ILLEGAL" },
            {TokenType.EOF, "EOF" },

            {TokenType.IDENT, "IDENT" },
            {TokenType.INT, "INT" },
            {TokenType.ASSIGN, "=" },
            {TokenType.PLUS, "+" },
            {TokenType.MINUS, "-" },
            {TokenType.BANG, "!" },
            {TokenType.ASTERISK, "*" },
            {TokenType.SLASH, "/" },
            {TokenType.LT, "<" },
            {TokenType.GT, ">" },
            {TokenType.EQ, "==" },
            {TokenType.NOT_EQ, "!=" },

            {TokenType.COMMA, "," },
            {TokenType.SEMICOLON, ";" },
            {TokenType.LPAREN, "(" },
            {TokenType.RPAREN, ")" },
            {TokenType.LBRACE, "{" },
            {TokenType.RBRACE, "}" },

            {TokenType.FUNCTION, "FUNCTION" },
            {TokenType.LET, "LET" },
            {TokenType.TRUE, "TRUE" },
            {TokenType.FALSE, "FALSE" },
            {TokenType.IF, "IF" },
            {TokenType.ELSE, "ELSE" },
            {TokenType.RETURN, "RETURN" },
        };

        public static readonly Dictionary<string, TokenType> reverseTokenMap = tokenMap.ToDictionary(x => x.Value, x => x.Key);

        public Token(TokenType type, string literal)
        {
            //var converted = reverseTokenMap.TryGetValue(type, out TokenType convertedType);
            //if (converted)
            //    Type = convertedType;
            //else
            //    throw new ArgumentException($"Could not find {type} in tokenMap");
            Type = type;
            Literal = literal;
        }

        public TokenType Type { get; set; }
        public string Literal { get; set; }

        public override string ToString()
        {
            return $"{{Type:{Type}, Literal:{Literal}}}";
        }
    }

    public enum TokenType
    {
        ILLEGAL = 0,
        EOF = 1,

        // Identifiers + literals
        IDENT = 50,
        INT = 51,

        // Operators
        ASSIGN = 100,
        PLUS = 101,
        MINUS = 102,
        BANG = 103,
        ASTERISK = 104,
        SLASH = 105,
        LT = 106,
        GT = 107,
        EQ = 108,
        NOT_EQ = 109,

        // Delimiters
        COMMA = 150,
        SEMICOLON = 151,
        LPAREN = 152,
        RPAREN = 153,
        LBRACE = 154,
        RBRACE = 155,

        // Keywords
        FUNCTION = 200,
        LET = 201,
        TRUE = 202,
        FALSE = 203,
        IF = 204,
        ELSE = 205,
        RETURN = 206,

    }
    //public static class TokenType
    //{
    //    public const string ILLEGAL = "ILLEGAL";
    //    public const string EOF = "EOF";
    //    public const string TRUE = "TRUE";
    //    public const string FALSE = "FALSE";
    //    public const string IF = "IF";
    //    public const string ELSE = "ELSE";
    //    public const string RETURN = "RETURN";

    //    public const string IDENT = "IDENT";
    //    public const string INT = "INT";

    //    public const string ASSIGN = "=";
    //    public const string PLUS = "+";
    //    public const string MINUS = "-";
    //    public const string BANG = "!";
    //    public const string ASTERISK = "*";
    //    public const string SLASH = "/";

    //    public const string LT = "<";
    //    public const string GT = ">";

    //    public const string EQ = "==";
    //    public const string NOT_EQ = "!=";

    //    public const string COMMA = ",";
    //    public const string SEMICOLON = ";";

    //    public const string LPAREN = "(";
    //    public const string RPAREN = ")";
    //    public const string LBRACE = "{";
    //    public const string RBRACE = "}";

    //    // Keywords
    //    public const string FUNCTION = "FUNCTION";
    //    public const string LET = "LET";

    //}
}
