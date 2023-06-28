module Lexer
    open Tokens
    type LexerState =
        {
            input : string
            mutable position : int
            mutable readPosition : int
            mutable ch : char
        }

    type ComplexTokenType =
        | Letter
        | Digit
        | Illegal

    let lookupIdent ident =
        if ident = "fn" then
            FUNCTION
        else if ident = "let" then
            LET
        else if ident = "if" then
            IF
        else if ident = "else" then
            ELSE
        else if ident = "return" then
            RETURN
        else if ident = "true" then
            TRUE
        else if ident = "false" then
            FALSE
        else
            IDENT

    let readChar (l: LexerState) =
        let newChar =
            match l.readPosition >= l.input.Length with
            | true -> '\000'
            | false -> l.input.Chars l.readPosition
        l.position <- l.readPosition
        l.readPosition <- l.readPosition + 1
        l.ch <- newChar

    let readString (l: LexerState) =
        let startPosition = l.position + 1
        
        readChar l
        while l.ch <> '"' && l.ch <> '\000' do
            readChar l

        l.input.Substring(startPosition, l.position - startPosition)

    let isLetter(ch: char) =
        let lowerCase = ch.CompareTo('a') >= 0 && ch.CompareTo('z') <= 0
        let upperCase = ch.CompareTo('A') >= 0 && ch.CompareTo('Z') <= 0;
        let underscore = ('_' = ch);
        lowerCase || upperCase || underscore

    let canReadLetter(l: LexerState) =
        //ensure I can read next position
        let canReadNextPosition = l.position + 1 < l.input.Length
        canReadNextPosition && isLetter(l.input.Chars(l.position + 1))

    let readIdentifier(l: LexerState) =
        let pos = l.position
        while canReadLetter(l) do
            readChar l
        let literal = l.input.Substring(pos, (l.position - pos + 1))
        let tokenType = lookupIdent literal
        (tokenType, literal)

    let isDigit(ch: char) =
        ch.CompareTo('0') >= 0 && ch.CompareTo('9') <= 0

    let canReadDigit(l: LexerState) =
        //ensure I can read next position
        let canReadNextPosition = l.position + 1 < l.input.Length
        canReadNextPosition && isDigit(l.input.Chars(l.position + 1)) 
        

    let readNumber(l: LexerState) =
        let pos = l.position
        while canReadDigit(l) do 
            readChar l
        let literal = l.input.Substring(pos, (l.position - pos + 1))
        (INT, literal)

    let peekChar(l : LexerState) =
        match l.readPosition >= l.input.Length with
        | true -> '\000'
        | false -> l.input.Chars l.readPosition
    
    let findComplexTokenType l =
        if isLetter(l.ch) then
            Letter
        else if isDigit(l.ch) then
            Digit
        else
            Illegal

    let nextComplexToken(l: LexerState) =
        match findComplexTokenType(l) with 
        | Letter -> readIdentifier(l)
        | Digit -> readNumber(l)
        | Illegal -> (TokenType.ILLEGAL, l.ch.ToString())

    let skipWhitespace(l: LexerState) =
        while l.ch = ' ' || l.ch = '\t' || l.ch = '\n' || l.ch = '\r' do
            readChar l
        ()

    let nextToken (l: LexerState) =

        skipWhitespace l

        let (tokenType, literal) =
            match l.ch with
            | '=' -> 
                let nextChar = peekChar l
                match nextChar with
                | '=' -> 
                    let ch = l.ch
                    readChar l
                    (TokenType.EQ, ch.ToString() + l.ch.ToString())
                | _ -> (TokenType.ASSIGN, l.ch.ToString())
            | '+' -> (TokenType.PLUS, l.ch.ToString())
            | '-' -> (TokenType.MINUS, l.ch.ToString())
            | '!' ->
                let nextChar = peekChar l
                match nextChar with
                | '=' ->
                    let ch = l.ch
                    readChar l
                    (TokenType.NOT_EQ, ch.ToString() + l.ch.ToString())
                | _ -> (TokenType.BANG, l.ch.ToString())
            | '*' -> (TokenType.ASTERISK, l.ch.ToString())
            | '/' -> (TokenType.SLASH, l.ch.ToString())
            | '<' -> (TokenType.LT, l.ch.ToString())
            | '>' -> (TokenType.GT, l.ch.ToString())
            | ',' -> (TokenType.COMMA, l.ch.ToString())
            | ';' -> (TokenType.SEMICOLON, l.ch.ToString())
            | '(' -> (TokenType.LPAREN, l.ch.ToString())
            | ')' -> (TokenType.RPAREN, l.ch.ToString())
            | '{' -> (TokenType.LBRACE, l.ch.ToString())
            | '}' -> (TokenType.RBRACE, l.ch.ToString())
            | '"' -> 
                let literal = readString l
                (TokenType.STRING, literal)
            | '\000' -> (TokenType.EOF, "")
            | _ -> nextComplexToken l

        let token = { TokenType = tokenType; Literal = literal }
        
        readChar l
        token
    
    let createLexer input =
        let lexer = { input = input; position = 0; readPosition = 0; ch = '\000'}
        readChar lexer
        lexer