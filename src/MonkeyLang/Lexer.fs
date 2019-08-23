module Lexer


open Tokens

type LexerState (input: string)=
    let _input = input
    let mutable _position = 0
    let mutable _readPosition = 0
    let mutable _ch = '\000'
    member l.input
        with get() = _input
    member l.position 
        with get() = _position
        and internal set v =
            _position <- v
    member l.readPosition
        with get() = _readPosition
        and internal set v =
            _readPosition <- v
    member l.ch
        with get() = _ch
        and internal set v =
            _ch <- v


type ComplexTokenType =
    | Letter
    | Digit
    | Illegal

/// Determines the TokenType of the string token
let internal lookupIdent ident =
    if ident = "fn" then Function
    else if ident = "let" then Let
    else if ident = "true" then True
    else if ident = "false" then False
    else if ident = "if" then If
    else if ident = "else" then Else
    else if ident = "return" then Return
    else Ident

/// Determines the TokenType of the number token
let internal lookupNumber number = Int

/// Determines whether the char is a letter
let internal isLetter(ch: char) =
    let lowerCase = ch.CompareTo('a') >= 0 && ch.CompareTo('z') <= 0
    let upperCase = ch.CompareTo('A') >= 0 && ch.CompareTo('Z') <= 0;
    let underscore = ('_' = ch);
    lowerCase || upperCase || underscore

/// Determines whether the char is a digit    
let internal isDigit(ch: char) =
    ch.CompareTo('0') >= 0 && ch.CompareTo('9') <= 0

/// Determines whether next char is part of current token
let private canReadNext(can:char -> bool) (l: LexerState) =
    match (l.position + 1) < l.input.Length with
    | true -> can(l.input.Chars(l.position + 1))
    | false -> false

/// Determines whether next char is part of current digit token
let internal canReadNextDigit(l: LexerState) = canReadNext isDigit l

/// Determines whether next char is part of current letter token
let internal canReadNextLetter(l: LexerState) = canReadNext isLetter l

/// Advances state to the next char in the input
let internal readChar (l: LexerState) =
    let newChar = 
        match l.readPosition >= l.input.Length with
        | true -> '\000'
        | false -> l.input.Chars l.readPosition
    l.ch <- newChar
    l.position <- l.readPosition
    l.readPosition <- l.readPosition + 1

/// Calculates the token
let private readChunk (canRead: LexerState -> bool) (lookupToken: string -> TokenType) (l: LexerState) =
    let pos = l.position
    while canRead(l) do
        readChar l
    let literal = l.input.Substring(pos, (l.position - pos + 1))
    let tokenType = lookupToken literal
    {TokenType = tokenType; Literal = literal}

/// Calculates the next number token
let internal readNumber(l: LexerState) = readChunk canReadNextDigit lookupNumber l

/// Calculates the next string token
let internal readIdentifier(l: LexerState) = readChunk canReadNextLetter lookupIdent l

/// Determines whether the next token is part of a valid ComplexTokenType or not
let internal determineComplexTokenType (l: LexerState) =
    if isLetter(l.ch) then Letter
    else if isDigit(l.ch) then Digit
    else Illegal

/// Calculates the next complex token
let internal nextComplexToken(l: LexerState) =
    match determineComplexTokenType(l) with 
    | Letter -> readIdentifier(l)
    | Digit -> readNumber(l)
    | Illegal -> {TokenType = TokenType.Illegal; Literal = l.ch.ToString()}
    
/// Skips any whitespace encountered in the input
let internal skipWhitespace(l: LexerState) =
    while l.ch = ' ' || l.ch = '\t' || l.ch = '\n' || l.ch = '\r' do
        readChar l
    
/// Reads the next available token in the input
let nextToken (l: LexerState) =

    skipWhitespace l

    let nextToken = 
        match l.ch with
        | '=' ->    { TokenType = TokenType.Assign; Literal = l.ch.ToString()}
        | '+' ->    { TokenType = TokenType.Plus; Literal = l.ch.ToString()}
        | ';' ->    { TokenType = TokenType.Semicolon; Literal = l.ch.ToString()}
        | '(' ->    { TokenType = TokenType.Lparen; Literal = l.ch.ToString()}
        | ')' ->    { TokenType = TokenType.Rparen; Literal = l.ch.ToString()}
        | '{' ->    { TokenType = TokenType.Lbrace; Literal = l.ch.ToString()}
        | '}' ->    { TokenType = TokenType.Rbrace; Literal = l.ch.ToString()}
        | ',' ->    { TokenType = TokenType.Comma; Literal = l.ch.ToString()}
        | '!' ->    { TokenType = TokenType.Bang; Literal = l.ch.ToString()}
        | '-' ->    { TokenType = TokenType.Minus; Literal = l.ch.ToString()}
        | '/' ->    { TokenType = TokenType.Slash; Literal = l.ch.ToString()}
        | '*' ->    { TokenType = TokenType.Asterik; Literal = l.ch.ToString()}
        | '<' ->    { TokenType = TokenType.Lt; Literal = l.ch.ToString()}
        | '>' ->    { TokenType = TokenType.Gt; Literal = l.ch.ToString()}
        | '\000' -> { TokenType = TokenType.Eof; Literal = ""}
        | _ ->      nextComplexToken(l)
    readChar l
    nextToken
    
/// Creates the Lexer
let createLexer input =
    let lexer = LexerState(input)
    readChar lexer
    lexer