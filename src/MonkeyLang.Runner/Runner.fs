module Runner
    open System.IO
    open Lexer
    open Parser
    open System



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
    let runProgram program =
        let lexer = createLexer program

        let parser = createParser lexer

        let program = parseProgram parser

        if parser.errors.Count > 0 then
            printParserErrors parser.errors |> ignore
            ()
        else
            let evaluated = Evaluator.evaluate program

            match evaluated with
            | Some obj -> 
                printfn "%s" (obj.Inspect())
            | None -> 
                ()
            ()

    let runFile filepath =
        if not (File.Exists(filepath)) then
            printfn "File (%s) does not exist, please run again with valid file" filepath
        else if Path.GetExtension(filepath) <> ".mon" then
            let extension = Path.GetExtension filepath
            printfn "Please pass a valid .mon file, got %s" extension
        else
            // let extension = Path.GetExtension filepath
            // printfn "Please pass a valid .mon file, got %s" extension
            let fullpath = Path.GetFullPath(filepath)

            let monkeyFile = File.ReadAllText fullpath
            //printfn "%s, %s, %s\n%s" ext filename filenamenoext fullpath

            //printfn "%s" monkeyFile
            //printfn "%s" "Running Mon"

            runProgram monkeyFile