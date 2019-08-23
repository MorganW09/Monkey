module Tokens

type TokenType =
    | ILLEGAL
    | EOF
    | IDENT         //identifiers
    | INT
    | ASSIGN        //operators
    | PLUS
    | COMMA         //delimiters
    | SEMICOLON
    | LPAREN       
    | RPAREN
    | LBRACE
    | RBRACE
    | FUNCTION      //keywords
    | LET

type Token =
    {
        TokenType : TokenType
        Literal : string
    }