module Tokens
    type TokenType =
        // special types
        | ILLEGAL
        | EOF
        // identifiers + literals
        | IDENT
        | INT
        | STRING
        // operators
        | ASSIGN
        | EQ
        | NOT_EQ
        | PLUS
        | MINUS
        | BANG
        | ASTERISK
        | SLASH
        | LT 
        | GT
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
        | TRUE
        | FALSE
        | IF
        | ELSE
        | RETURN
    
    type Token =
        {
            TokenType : TokenType
            Literal : string
        }
