module Parser

open Lexer

type Parser (lexerState : LexerState) =
    let _lexerState = lexerState