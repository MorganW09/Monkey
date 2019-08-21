using System;
using System.Collections.Generic;
using System.Text;

namespace Monkey
{
    public enum TokenType
    {
        ILLEGAL,
        EOF,

        //identifiers + literals
        IDENT, // add, foobar, x, y
        INT,//1343456
                
        //operators
        ASSIGN,
        PLUS,
                
        //delimiters
        COMMA,
        SEMICOLON,

        LPAREN,
        RPAREN,
        LBRACE,
        RBRACE,

        // keywords        
        FUNCTION,
        LET
    }
}
