module Tokens

[<Literal>]
let ILLEGAL = "ILLEGAL"
[<Literal>]
let EOF = "EOF"
[<Literal>]
let IDENT = "IDENT"
[<Literal>]
let INT = "INT"
[<Literal>]
let ASSIGN = "="
[<Literal>]
let PLUS = "+"
[<Literal>]
let MINUS = "-"
[<Literal>]
let BANG = "!"
[<Literal>]
let ASTERIK = "*"
[<Literal>]
let SLASH = "/"
[<Literal>]
let LT = "<"
[<Literal>]
let GT = ">"
[<Literal>]
let EQ = "=="
[<Literal>]
let NOT_EQ = "!="
[<Literal>]
let COMMA = ","
[<Literal>]
let SEMICOLON = ";"
[<Literal>]
let LPAREN = "("
[<Literal>]
let RPAREN = ")"
[<Literal>]
let LBRACE = "{"
[<Literal>]
let RBRACE = "}"
[<Literal>]
let FUNCTION = "FUNCTION"
[<Literal>]
let LET = "LET"
[<Literal>]
let TRUE = "TRUE"
[<Literal>]
let FALSE = "FALSE"
[<Literal>]
let RETURN = "RETURN"
[<Literal>]
let ELSE = "ELSE"
[<Literal>]
let IF = "IF"

type TokenType =
    | Illegal
    | Eof
    | Ident         //identifiers
    | Int
    | Assign        //operators
    | Plus
    | Minus
    | Bang 
    | Asterik 
    | Slash 
    | Gt 
    | Lt 
    | Eq 
    | Not_Eq
    | Comma         //delimiters
    | Semicolon
    | Lparen       
    | Rparen
    | Lbrace
    | Rbrace
    | Function      //keywords
    | Let
    | True 
    | False 
    | If 
    | Else 
    | Return

type Token =
    {
        TokenType : TokenType
        Literal : string
    }