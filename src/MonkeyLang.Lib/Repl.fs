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

        let env = new Object.Environment(None)
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
                let evaluated = Evaluator.eval program env

                match evaluated with
                | Some obj -> 
                    printfn "%s" (obj.Inspect())
                | None -> 
                    ()
                ()