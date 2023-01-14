module Tokens
    type TokenType =
        // special types
        | ILLEGAL
        | EOF
        // identifiers + literals
        | IDENT
        | INT
        // operators
        | ASSIGN
        | PLUS
        // delimiters
        | COMMA
        | SEMICOLON
        | LPAREN
        | RPAREN
        | LBRACE
        | RBRACE
        // keywords
        | FUNCTION
        | LET
    
    type Token =
        {
            TokenType : TokenType
            Literal : string
        }
