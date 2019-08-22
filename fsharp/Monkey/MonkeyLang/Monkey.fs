module Monkey

open Tokens

//type Lexer (input : string, position : int, readPosition : int, ch : char) =
//    member l.input = input
//    member l.position = position
//    member l.readPosition = readPosition
//    member l.ch = ch
type LexerState =
    {
        input : string
        mutable position : int
        mutable readPosition : int
        mutable ch : char
    }

//type Keywords =
//    | FN
//    | LET
type ComplexTokenType =
    | Letter
    | Digit
    | Illegal

let lookupIdent ident =
    if ident = "fn" then
        FUNCTION
    else if ident = "let" then
        LET
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

let isLetter(ch: char) =
    let lowerCase = ch.CompareTo('a') >= 0 && ch.CompareTo('z') <= 0
    let upperCase = ch.CompareTo('A') >= 0 && ch.CompareTo('Z') <= 0;
    let underscore = ('_' = ch);
    lowerCase || upperCase || underscore

let canReadLetter(l: LexerState) =
    isLetter(l.input.Chars(l.position + 1))

let isDigit(ch: char) =
    ch.CompareTo('0') >= 0 && ch.CompareTo('9') <= 0

let canReadDigit(l: LexerState) =
    isDigit(l.input.Chars(l.position + 1))

let findComplexTokenType l =
    if isLetter(l.ch) then
        Letter
    else if isDigit(l.ch) then
        Digit
    else
        Illegal

let readNumber(l: LexerState) =
    let pos = l.position
    while canReadDigit(l) do 
        readChar l
    let literal = l.input.Substring(pos, (l.position - pos + 1))
    {TokenType = INT; Literal = literal}

let readIdentifier(l: LexerState) =
    let pos = l.position
    while canReadLetter(l) do
        readChar l
    let literal = l.input.Substring(pos, (l.position - pos + 1))
    let tokenType = lookupIdent literal
    {TokenType = tokenType; Literal = literal}

let nextComplexToken(l: LexerState) =
    match findComplexTokenType(l) with 
    | Letter -> readIdentifier(l)
    | Digit -> readNumber(l)
    | Illegal -> {TokenType = TokenType.ILLEGAL; Literal = l.ch.ToString()}
    
let skipWhitespace(l: LexerState) =
    while l.ch = ' ' || l.ch = '\t' || l.ch = '\n' || l.ch = '\r' do
        readChar l
    ()
let nextToken (l: LexerState) =

    skipWhitespace l

    let nextToken = 
        match l.ch with
        | '=' ->    { TokenType = TokenType.ASSIGN; Literal = l.ch.ToString()}
        | '+' ->    { TokenType = TokenType.PLUS; Literal = l.ch.ToString()}
        | ';' ->    { TokenType = TokenType.SEMICOLON; Literal = l.ch.ToString()}
        | '(' ->    { TokenType = TokenType.LPAREN; Literal = l.ch.ToString()}
        | ')' ->    { TokenType = TokenType.RPAREN; Literal = l.ch.ToString()}
        | '{' ->    { TokenType = TokenType.LBRACE; Literal = l.ch.ToString()}
        | '}' ->    { TokenType = TokenType.RBRACE; Literal = l.ch.ToString()}
        | ',' ->    { TokenType = TokenType.COMMA; Literal = l.ch.ToString()}
        | '\000' -> { TokenType = TokenType.EOF; Literal = ""}
        | _ ->      nextComplexToken(l)
    readChar l
    nextToken
    
let createLexer input =
    let lexer = {input = input; position = 0; readPosition = 0; ch = '\000'}
    readChar lexer
    lexer