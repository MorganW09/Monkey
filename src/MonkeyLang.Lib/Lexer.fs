module Lexer
    open Tokens
    type LexerState =
        {
            input : string
            mutable position : int
            mutable readPosition : int
            mutable ch : char
        }

    let readChar (l: LexerState) =
        let newChar =
            match l.readPosition >= l.input.Length with
            | true -> '\000'
            | false -> l.input.Chars l.readPosition
        l.position <- l.readPosition
        l.readPosition <- l.readPosition + 1
        l.ch <- newChar

    let nextToken (l: LexerState) =
        let tokenType =
            match l.ch with
            | '=' -> TokenType.ASSIGN
            | ';' -> TokenType.SEMICOLON
            | '(' -> TokenType.LPAREN
            | ')' -> TokenType.RPAREN
            | ',' -> TokenType.COMMA
            | '+' -> TokenType.PLUS
            | '{' -> TokenType.LBRACE
            | '}' -> TokenType.RBRACE
            | _ -> TokenType.EOF
        
        let literal =
            match tokenType with
            | TokenType.EOF -> ""
            | _ -> l.ch.ToString()

        let token = { TokenType = tokenType; Literal = literal }
        
        readChar l
        token
    
    let createLexer input =
        let lexer = { input = input; position = 0; readPosition = 0; ch = '\000'}
        readChar lexer
        lexer