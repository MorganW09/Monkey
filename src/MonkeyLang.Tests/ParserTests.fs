module ParserTests

open Xunit
open Lexer
open Parser

let canDowncastToLetStatement (s: Ast.Statement) =
    match s with 
    | :? Ast.LetStatement as ls -> true
    | _ -> false
let canDowncastToReturnStatement (s: Ast.Statement) =
    match s with 
    | :? Ast.ReturnStatement as rs -> true
    | _ -> false
let canDowncastToExpressionStatement (s: Ast.Statement) =
    match s with
    | :? Ast.ExpressionStatement as es -> true
    | _ -> false
let isIdentifier (e : Ast.Expression) =
    match e with
    | :? Ast.Identifier as id -> true
    | _ -> false


let testLetStatement (expected: string) (s: Ast.Statement) =
    Assert.Equal(s.TokenLiteral(), "let")

    Assert.True(canDowncastToLetStatement s)

    let ls = s :?> Ast.LetStatement

    let nameExpression = (ls.name :> Ast.Expression)

    Assert.Equal(expected, ls.name.value)
    Assert.Equal(expected, nameExpression.TokenLiteral())

    //TODO - need to add something to test value expression

let testReturnStatement (expected: string) (s: Ast.Statement) =
    Assert.Equal("return", s.TokenLiteral())

    Assert.True(canDowncastToReturnStatement s)
    //TODO - add test for expected expression at some point
    //let rs = s :?> Ast.ReturnStatement

let AssertNoParseErrors (p: ParserState) =
    //if errors, maybe print to err out
    //TODO - maybe include parser errors in the output
    //TODO - maybe print to stderr
    Assert.Equal(0, p.errors.Count)

[<Fact>]
let ``Can Parse Let Statement`` () =
    let input = "let x = 5;
    let y = 10;
    let foobar = 838383;"

    let lexer = createLexer input

    let parser = createParser lexer

    let program = parseProgram parser
    
    AssertNoParseErrors parser

    let expectedStatements = [| "x"; "y"; "foobar" |]

    Assert.Equal (3, program.statements.Length)

    Array.zip expectedStatements program.statements
        |> Array.map (fun (e, a) -> testLetStatement e a)

[<Fact>]
let ``Can Test expectPeek`` () =
    let input = "let x = 5;"
    
    let lexer = createLexer input

    let parser = createParser lexer

    let isIdent = expectPeek parser Tokens.TokenType.IDENT

    Assert.True(isIdent);

[<Fact>]
let ``Can generate peek errors`` () =
    let input = "let x 5;
let = 10;
let 838383;"

    let lexer = createLexer input

    let parser = createParser lexer

    let program = parseProgram parser

    Assert.Equal(4, parser.errors.Count)
    
    let expectedErrors = [| 
        "expected next token to be ASSIGN, got INT instead"; 
        "expected next token to be IDENT, got ASSIGN instead"; 
        "expected next token to be IDENT, got INT instead"; 
        "expected next token to be ASSIGN, got INT instead"; 
         |]

    let errors = parser.errors.ToArray()
    Array.zip expectedErrors errors
        |> Array.map (fun (e, a) -> Assert.Equal(e, a))

// TODO - maybe get this working
// [<Fact>]
// let ``Can include parser errors on output`` () =
//     let input = "let x 5;
// let = 10;
// let 838383;"

//     let lexer = createLexer input

//     let parser = createParser lexer

//     let program = parseProgram parser
    
//     AssertNoParseErrors parser

[<Fact>]
let ``Can parse return statements`` () =
    let input = "return 5;
return 10;
return 993322;"

    let lexer = createLexer input

    let parser = createParser lexer

    let program = parseProgram parser
    
    AssertNoParseErrors parser

    let expectedStatements = [| "x"; "y"; "foobar" |]

    Assert.Equal (3, program.statements.Length)

    Array.zip expectedStatements program.statements
        |> Array.map (fun (e, a) -> testReturnStatement e a)

[<Fact>]
let ``Can test identifier expression`` () =
    let input = "foobar;"

    let lexer = createLexer input
    let parser = createParser lexer
    let program = parseProgram parser

    AssertNoParseErrors parser

    Assert.Equal(1, program.statements.Length)

    Assert.True(canDowncastToExpressionStatement(program.statements.[0]))

    let es = program.statements.[0] :?> Ast.ExpressionStatement

    Assert.True(isIdentifier(es.expression))

    let identifier = (es.expression :?> Ast.Identifier)

    Assert.Equal("foobar", identifier.value)
    Assert.Equal("foobar", es.expression.TokenLiteral())