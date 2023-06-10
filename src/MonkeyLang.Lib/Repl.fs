module Repl
    open System
    open Lexer
    open Tokens
    let PROMPT = ">> "

    let start () =
        Console.Write(PROMPT)
        let line = Console.ReadLine()

        let lexer = createLexer line

        let mutable stillHasTokens = true
        while stillHasTokens do
            let actualToken = nextToken lexer

            printfn "(TokenType: %s, Literal: %s" (actualToken.TokenType.ToString()) actualToken.Literal

            if (actualToken.TokenType = TokenType.EOF) then
                stillHasTokens <- false