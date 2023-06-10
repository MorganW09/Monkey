// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =    
    printfn("Hello! This is the Monkey programming language!")
    printfn("Feel free to type in commands")
    Repl.start()
    0
