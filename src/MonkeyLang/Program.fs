// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    printfn "Hello World from MonkeyLang!"
    while (true) do
        printf ">>>"
        let input = Console.ReadLine()
        let lexer = Lexer.createLexer input

        while lexer.ch <> '\000' do        
            let token = Lexer.nextToken lexer    
            let tokenString = token.ToString()
            printfn "%s" tokenString
    0
