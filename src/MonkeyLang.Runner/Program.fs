// Learn more about F# at http://fsharp.org

open System
open System.IO

[<EntryPoint>]
let main argv =
    if argv.Length = 0 then
        printfn("Welcome to the Monkey Language.")
        printfn("Please invoke Monkey with either <file> or <repl> command.")
        printfn("")
        printfn("Usage:")
        printfn("    dotnet run file [filepath]")
        printfn("    dotnet run repl")
        printfn("")
        printfn("Options:")
        printfn("    file [filepath]- reads in a Monkey file (.mon) and returns results")
        printfn("    repl - launches a repl to evaluated Monkey code on the fly")
    else
        let firstParam = argv.[0]

        match firstParam with
        | "repl" -> 
            Repl.start()
        | "file" ->
            if argv.Length <> 2 then
                printfn "Please pass a file path when using the \"file\" command"
            else
                let filepath = argv.[1]

                Runner.runFile filepath
        | _ ->
            printfn "Unknown command: \"%s\"" firstParam
            printfn "Please fix command and try again"

    // printfn("Hello! This is the Monkey programming language!")
    // printfn("Feel free to type in commands")
    // Repl.start()
    0
