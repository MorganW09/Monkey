module ParserTests

open Xunit
open Lexer
open Parser

let canDowncastToLetStatement (s: Ast.Statement) =
    match s with 
    | :? Ast.LetStatement as ls -> true
    | _ -> false

let testLetStatement (expected: string) (s: Ast.Statement) =
    Assert.Equal(s.TokenLiteral(), "let")

    Assert.True(canDowncastToLetStatement s)

    let ls = s :?> Ast.LetStatement

    let nameExpression = (ls.name :> Ast.Expression)

    Assert.Equal(expected, ls.name.value)
    Assert.Equal(expected, nameExpression.TokenLiteral())

[<Fact>]
let ``Can Test Let Statement`` () =
    let input = "let x = 5;
    let y = 10;
    let foobar = 838383;"
    //let input = "let x = 5;"
    

    let lexer = createLexer input

    let parser = createParser lexer

    let program = parseProgram parser

    let expectedStatements = [| "x"; "y"; "foobar" |]
    //let expectedStatements = [| "x" |]

    Assert.Equal (3, program.Statements.Length)

    // let tokenLiterals = 
    //     program.Statements 
    //     |> Array.map (fun s -> s.TokenLiteral())

    Array.zip expectedStatements program.Statements
        |> Array.map (fun (e, a) -> testLetStatement e a)

[<Fact>]
let ``Can Test expectPeek`` () =
    // let input = "let x = 5;
    // let y = 10;
    // let foobar = 838383;"
    let input = "let x = 5;"
    

    let lexer = createLexer input

    let parser = createParser lexer

    let isIdent = expectPeek parser Tokens.TokenType.IDENT

    Assert.True(isIdent);