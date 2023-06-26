module Repl
    open System
    open Lexer
    open Parser
    open Tokens
    let PROMPT = ">> "

    let MONKEY_FACE = " 
         __,__
 .--. .-'     '-. .--.
/ .. \/ .-. .-. \/ .. \ 
| | '| /   Y   \ |' | |
| \  \ \ 0 | 0 / /  / |
\ '- ,\.-'''''-./, -' /
''-'  /_  ^ ^  _\  '-''
     |  \._ _./  |
     \   /\'~'/   /
      '._'-=-'_.'
        '-----'"

    let printParserErrors (errors: ResizeArray<string>) =
        Console.WriteLine (MONKEY_FACE)
        Console.WriteLine ("Woops! We ran into some monkey business here!")
        errors.ToArray()
            |> Array.map (fun e -> printfn "\t%s" e)

    let start () =

        while true do
            Console.Write(PROMPT)
            let line = Console.ReadLine()

            let lexer = createLexer line

            let parser = createParser lexer

            let program = parseProgram parser

            if parser.errors.Count > 0 then
                printParserErrors parser.errors |> ignore
                ()
            else
                let evaluated = Evaluator.eval program
                //let programStr = (program :> Ast.Node).Str()
                //printfn "%s" programStr

                match evaluated with
                | Some obj -> 
                    printfn "%s" (obj.Inspect())
                | None -> 
                    printfn "Could not evaluate %s" line
                ()
            // let mutable stillHasTokens = true
            // while stillHasTokens do
            //     let actualToken = nextToken lexer

            //     printfn "(TokenType: %s, Literal: %s" (actualToken.TokenType.ToString()) actualToken.Literal

            //     if (actualToken.TokenType = TokenType.EOF) then
            //         stillHasTokens <- false