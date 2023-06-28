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
let isInteger (e : Ast.Expression) =
    match e with
    | :? Ast.IntegerLiteral as id -> true
    | _ -> false
let isBoolean (e: Ast.Expression) =
    match e with
    | :? Ast.Boolean as b -> true
    | _ -> false
let canDowncastToPrefixExpression (s: Ast.Expression) =
    match s with
    | :? Ast.PrefixExpression as pe -> true
    | _ -> false
let canDowncastToInfixExpression (s: Ast.Expression) =
    match s with
    | :? Ast.InfixExpression as ie -> true
    | _ -> false
let canDowncastToIfExpression (s: Ast.Expression) =
    match s with
    | :? Ast.IfExpression as ie -> true
    | _ -> false
let canDowncastToFunctionLiteral (s: Ast.Expression) =
    match s with
    | :? Ast.FunctionLiteral as fl -> true
    | _ -> false
let canDowncastToCallExpression (s: Ast.Expression) =
    match s with
    | :? Ast.CallExpression as ce -> true
    | _ -> false
let canDowncastToStringLiteral (s: Ast.Expression) =
    match s with
    | :? Ast.StringLiteral as ce -> true
    | _ -> false
let canDowncastToExpression (s: Ast.Statement) =
    match s with
    | :? Ast.Expression as ce -> true
    | _ -> false

let testLetStatement (expected: string) (s: Ast.Statement) =
    Assert.Equal(s.TokenLiteral(), "let")

    Assert.True(canDowncastToLetStatement s)

    let ls = s :?> Ast.LetStatement

    let nameExpression = (ls.name :> Ast.Expression)

    Assert.Equal(expected, ls.name.value)
    Assert.Equal(expected, nameExpression.TokenLiteral())

    //TODO - need to add something to test value expression

let testReturnStatement (expected: int64) (s: Ast.Statement) =
    Assert.Equal("return", s.TokenLiteral())

    Assert.True(canDowncastToReturnStatement s)
    //TODO - add test for expected expression at some point
    let rs = s :?> Ast.ReturnStatement

    Assert.True(isInteger rs.returnValue, "isIdentifier")

    let identifier = rs.returnValue :?> Ast.IntegerLiteral

    Assert.Equal(expected, identifier.value)

let AssertNoParseErrors (p: ParserState) =
    //if errors, maybe print to err out
    //TODO - maybe include parser errors in the output
    //TODO - maybe print to stderr
    let errorMessage =  if p.errors.Count > 0 then 
                            let error = p.errors.ToArray() |> Array.reduce (fun a b -> sprintf "%s\n%s" a b)
                            error
                        else ""
    Assert.True(0 = p.errors.Count, errorMessage)

let testIntegerLiteral (il : Ast.Expression) (value) =
    Assert.True(isInteger il)

    let integerLiteral = il :?> Ast.IntegerLiteral

    Assert.Equal(integerLiteral.value, value)

    Assert.Equal(il.TokenLiteral(), (sprintf "%d" value))

let testBoolean (il : Ast.Expression) (value) =
    Assert.True(isBoolean il)

    let boolean = il :?> Ast.Boolean

    Assert.Equal(boolean.value, value)
    Assert.Equal(il.TokenLiteral(), (sprintf "%b" value))

let testIntInfixExpression (ie : Ast.Expression) (left) (operator) (right) =
    Assert.True(canDowncastToInfixExpression ie)

    let infixExpr = ie :?> Ast.InfixExpression

    testIntegerLiteral infixExpr.left left

    Assert.Equal(operator, infixExpr.operator)

    testIntegerLiteral infixExpr.right right

let testIdentifier (ie: Ast.Expression) (value) =
    Assert.True(isIdentifier ie, "Is identifier")
    
    let identifier = ie :?> Ast.Identifier

    let valueEqual = value = identifier.value
    Assert.True(valueEqual, "testing values")
    Assert.Equal(value, ie.TokenLiteral())

let testStrInfixExpression (ie : Ast.Expression) (left) (operator) (right) =
    Assert.True(canDowncastToInfixExpression ie)

    let infixExpr = ie :?> Ast.InfixExpression

    testIdentifier infixExpr.left left

    Assert.Equal(operator, infixExpr.operator)

    testIdentifier infixExpr.right right

let testStringLiteral (il : Ast.Statement) (value) =
    let typeStr = il.GetType().ToString()
    Assert.True(canDowncastToExpressionStatement il, (sprintf "Cannot downcast %s to expression" typeStr))

    let expr = il :?> Ast.ExpressionStatement

    Assert.True(canDowncastToStringLiteral expr.expression)

    let stringLiteral = expr.expression :?> Ast.StringLiteral

    Assert.Equal(stringLiteral.value, value)

let assertBasicStuff input =
    let lexer = createLexer input
    let parser = createParser lexer
    let program = parseProgram parser

    AssertNoParseErrors parser

    Assert.Equal(1, program.statements.Length)

    program

let assertExpressionStatement (program : Ast.Program) =
    
    Assert.True(canDowncastToExpressionStatement(program.statements.[0]))

    let es = program.statements.[0] :?> Ast.ExpressionStatement

    es

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
let ``Can generate simple peek error`` () =
    let input = "let x 5;"

    let lexer = createLexer input

    let parser = createParser lexer

    let program = parseProgram parser

    Assert.Equal(1, parser.errors.Count)
    
    let expectedErrors = [| 
        "expected next token to be ASSIGN, got INT instead"; 
         |]

    let errors = parser.errors.ToArray()
    Array.zip expectedErrors errors
        |> Array.map (fun (e, a) -> Assert.Equal(e, a))

[<Fact>]
let ``Can generate more complex peek error`` () =
    let input = "let = 10;"

    let lexer = createLexer input

    let parser = createParser lexer

    let program = parseProgram parser

    Assert.Equal(2, parser.errors.Count)
    
    let expectedErrors = [| 
        "expected next token to be IDENT, got ASSIGN instead"; 
        "no prefix parse function for = found"; 
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

    let expectedStatements = [| 5L; 10L; 993322L |]

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

    testIdentifier es.expression "foobar"

[<Fact>]
let ``Can test integer expression`` () =
    let input = "5;"

    let lexer = createLexer input
    let parser = createParser lexer
    let program = parseProgram parser

    AssertNoParseErrors parser

    Assert.Equal(1, program.statements.Length)

    Assert.True(canDowncastToExpressionStatement(program.statements.[0]))

    let es = program.statements.[0] :?> Ast.ExpressionStatement

    testIntegerLiteral es.expression 5L

//TODO - maybe turn this into a theory, check book
[<Fact>]
let ``Can test prefix expression parsing`` () =
    let input = "!5"

    let lexer = createLexer input
    let parser = createParser lexer
    let program = parseProgram parser

    AssertNoParseErrors parser

    Assert.Equal(1, program.statements.Length)

    Assert.True(canDowncastToExpressionStatement(program.statements.[0]), "cannot downcast to expression statement")

    let es = program.statements.[0] :?> Ast.ExpressionStatement

    Assert.True(canDowncastToPrefixExpression(es.expression), "cannot downcast to prefix expression")

    let pe = es.expression :?> Ast.PrefixExpression
    
    Assert.Equal("!", pe.operator)
    testIntegerLiteral pe.right 5L

[<Theory>]
[<InlineData("!true;", "!", true)>]
[<InlineData("!false;", "!", false)>]
let ``Can test boolean prefix expressions`` (input: string) (exOp: string) (exBoolean: bool) =

    let lexer = createLexer input
    let parser = createParser lexer
    let program = parseProgram parser

    AssertNoParseErrors parser

    Assert.Equal(1, program.statements.Length)

    Assert.True(canDowncastToExpressionStatement(program.statements.[0]), "cannot downcast to expression statement")

    let es = program.statements.[0] :?> Ast.ExpressionStatement

    Assert.True(canDowncastToPrefixExpression(es.expression), "cannot downcast to prefix expression")

    let pe = es.expression :?> Ast.PrefixExpression
    
    Assert.Equal("!", pe.operator)
    testBoolean pe.right exBoolean

[<Theory>]
[<InlineData("5 + 6;", 5, "+", 6)>]
[<InlineData("5 - 6;", 5, "-", 6)>]
[<InlineData("5 * 6;", 5, "*", 6)>]
[<InlineData("5 / 6;", 5, "/", 6)>]
[<InlineData("5 > 6;", 5, ">", 6)>]
[<InlineData("5 < 6;", 5, "<", 6)>]
[<InlineData("5 == 6;", 5, "==", 6)>]
[<InlineData("5 != 6;", 5, "!=", 6)>]
let ``Can test integer infix operations`` input exLeft expectedOp exRight =
    let lexer = createLexer input
    let parser = createParser lexer
    let program = parseProgram parser

    AssertNoParseErrors parser

    Assert.Equal(1, program.statements.Length)

    Assert.True(canDowncastToExpressionStatement(program.statements.[0]), "cannot downcast to expression statement")

    let es = program.statements.[0] :?> Ast.ExpressionStatement

    Assert.True(canDowncastToInfixExpression(es.expression), "cannot downcast to prefix expression")

    let ie = es.expression :?> Ast.InfixExpression

    testIntegerLiteral ie.left exLeft
    Assert.Equal(expectedOp, ie.operator)
    testIntegerLiteral ie.right exRight

[<Theory>]
[<InlineData("-a * b;", "((-a) * b)")>]
[<InlineData("!-a", "(!(-a))")>]
[<InlineData("a + b + c", "((a + b) + c)")>]
[<InlineData("a + b - c","((a + b) - c)")>]
[<InlineData("a * b * c","((a * b) * c)")>]
[<InlineData("a * b / c","((a * b) / c)")>]
[<InlineData("a + b / c","(a + (b / c))")>]
[<InlineData("a + b * c + d / e - f","(((a + (b * c)) + (d / e)) - f)")>]
[<InlineData("3 + 4; -5 * 5","(3 + 4)((-5) * 5)")>]
[<InlineData("5 > 4 == 3 < 4","((5 > 4) == (3 < 4))")>]
[<InlineData("5 < 4 != 3 > 4","((5 < 4) != (3 > 4))")>]
[<InlineData("3 + 4 * 5 == 3 * 1 + 4 * 5","((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))")>]
[<InlineData("true", "true")>]
[<InlineData("false", "false")>]
[<InlineData("3 > 5 == false", "((3 > 5) == false)")>]
[<InlineData("3 < 5 == true", "((3 < 5) == true)")>]
[<InlineData("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)")>]
[<InlineData("(5 + 5) * 2","((5 + 5) * 2)")>]
[<InlineData("2 / (5 + 5)","(2 / (5 + 5))")>]
[<InlineData("-(5 + 5)","(-(5 + 5))")>]
[<InlineData("!(true == true)","(!(true == true))")>]
[<InlineData("a + add(b * c) + d","((a + add((b * c))) + d)")>]
[<InlineData("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))","add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))")>]
[<InlineData("add(a + b + c * d / f + g)","add((((a + b) + ((c * d) / f)) + g))")>]
let ``Can test operator precedence`` input expected =
    let lexer = createLexer input
    let parser = createParser lexer
    let program = parseProgram parser

    AssertNoParseErrors parser

    Assert.Equal(expected, (program :> Ast.Node).Str())

[<Fact>]
let ``Can test boolean expression`` () =
    let input = "true;"

    let lexer = createLexer input
    let parser = createParser lexer
    let program = parseProgram parser

    AssertNoParseErrors parser

    Assert.Equal(1, program.statements.Length)

    Assert.True(canDowncastToExpressionStatement(program.statements.[0]))

    let es = program.statements.[0] :?> Ast.ExpressionStatement

    testBoolean es.expression true

[<Theory>]
[<InlineData("true == true", true, "==", true)>]
[<InlineData("true != false", true, "!=", false)>]
[<InlineData("false == false", false, "==", false)>]
let ``Can test boolean infix expressions`` input expectedLeft expectedOp expectedRight =
    let lexer = createLexer input
    let parser = createParser lexer
    let program = parseProgram parser

    AssertNoParseErrors parser

    Assert.Equal(1, program.statements.Length)

    Assert.True(canDowncastToExpressionStatement(program.statements.[0]))

    let es = program.statements.[0] :?> Ast.ExpressionStatement

    Assert.True(canDowncastToInfixExpression(es.expression))

    let infixExpr = es.expression :?> Ast.InfixExpression
    
    testBoolean infixExpr.left expectedLeft
    Assert.Equal(expectedOp, infixExpr.operator)
    testBoolean infixExpr.right expectedRight

[<Fact>]
let ``Can test if expression`` () =
    let input = "if (x < y) { x }"
    
    let lexer = createLexer input
    let parser = createParser lexer
    let program = parseProgram parser

    AssertNoParseErrors parser

    Assert.Equal(1, program.statements.Length)

    Assert.True(canDowncastToExpressionStatement(program.statements.[0]))

    let es = program.statements.[0] :?> Ast.ExpressionStatement

    Assert.True(canDowncastToIfExpression(es.expression))

    let ifExpr = es.expression :?> Ast.IfExpression

    testStrInfixExpression ifExpr.condition "x" "<" "y"

    Assert.Equal(1, ifExpr.consequence.statements.Length)

    let consequence = ifExpr.consequence.statements.[0]
    Assert.True(canDowncastToExpressionStatement(consequence))

    let conExpr = consequence :?> Ast.ExpressionStatement

    testIdentifier conExpr.expression "x"

    Assert.True(ifExpr.alternative.IsNone)

//test if/else expression

[<Fact>]
let ``Can test if else expression`` () =
    let input = "if (x < y) { x } else { y }"
    
    let lexer = createLexer input
    let parser = createParser lexer
    let program = parseProgram parser

    AssertNoParseErrors parser

    Assert.Equal(1, program.statements.Length)

    Assert.True(canDowncastToExpressionStatement(program.statements.[0]))

    let es = program.statements.[0] :?> Ast.ExpressionStatement

    Assert.True(canDowncastToIfExpression(es.expression))

    let ifExpr = es.expression :?> Ast.IfExpression

    Assert.True(ifExpr.alternative.IsSome)

    let alternative = ifExpr.alternative.Value

    Assert.Equal(1, alternative.statements.Length)

    let alternativeStatement = alternative.statements.[0]
    Assert.True(canDowncastToExpressionStatement(alternativeStatement))

    let altExpr = alternativeStatement :?> Ast.ExpressionStatement

    testIdentifier altExpr.expression "y"

[<Fact>]
let ``CanTestFunctionalLiteralParsing`` () =
    let input = "fn(x, y) { x + y; }"
    
    let lexer = createLexer input
    let parser = createParser lexer
    let program = parseProgram parser
    
    Assert.Equal(1, program.statements.Length)

    AssertNoParseErrors parser


    Assert.True(canDowncastToExpressionStatement(program.statements.[0]))

    let es = program.statements.[0] :?> Ast.ExpressionStatement

    Assert.True(canDowncastToFunctionLiteral es.expression)

    let fnLit = es.expression :?> Ast.FunctionLiteral

    Assert.Equal(2, fnLit.parameters.Length)

    testIdentifier fnLit.parameters.[0] "x"
    testIdentifier fnLit.parameters.[1] "y"

    Assert.Equal(1, fnLit.body.statements.Length)

    Assert.True(canDowncastToExpressionStatement(fnLit.body.statements.[0]))

    let bodyStmt = fnLit.body.statements.[0] :?> Ast.ExpressionStatement


    testStrInfixExpression bodyStmt.expression "x" "+" "y"



[<Theory>]
[<InlineData("fn () {};", 0)>]
[<InlineData("fn (x) {};", 1)>]
[<InlineData("fn (x, y, z) {};", 2)>]
let ``Can test function parameter parsing`` input number =
    let expectedParameters =
        if number = 0 then Array.empty<string>
        else if number = 1 then [| "x" |]
        else [| "x"; "y"; "z" |]
    
    // let lexer = createLexer input
    // let parser = createParser lexer
    // let program = parseProgram parser

    // AssertNoParseErrors parser

    // Assert.Equal(1, program.statements.Length)

    // Assert.True(canDowncastToExpressionStatement(program.statements.[0]))

    let program = assertBasicStuff input
    
    let es = assertExpressionStatement program

    Assert.True(canDowncastToFunctionLiteral es.expression)

    let fnLit = es.expression :?> Ast.FunctionLiteral

    Assert.Equal(expectedParameters.Length, fnLit.parameters.Length)
    
    Array.zip expectedParameters fnLit.parameters
        |> Array.map (fun (e, a) -> testIdentifier a e)

[<Fact>]
let ``Can test call expression parsing`` () =
    let input = "add(1, 2 * 3, 4 + 5);"

    let program = assertBasicStuff input
    
    let es = assertExpressionStatement program
    
    Assert.True(canDowncastToCallExpression es.expression)

    let callExpr = es.expression :?> Ast.CallExpression

    testIdentifier callExpr.func "add"

    Assert.Equal(3, callExpr.arguments.Length)

    testIntegerLiteral callExpr.arguments.[0] 1L
    testIntInfixExpression callExpr.arguments.[1] 2L "*" 3L
    testIntInfixExpression callExpr.arguments.[2] 4L "+" 5L

[<Theory>]
[<InlineData("add();", "add", 0)>]
[<InlineData("add(1);", "add", 1)>]
[<InlineData("add(1, 2 * 3, 4 + 5);", "add", 2)>]
let ``Can test call expression parameter parsing`` input exIdent num =
    let expectedParameters =
        if num = 0 then Array.empty<string>
        else if num = 1 then [| "1" |]
        else [| "1"; "(2 * 3)"; "(4 + 5)" |]

    let program = assertBasicStuff input
    
    let es = assertExpressionStatement program

    Assert.True(canDowncastToCallExpression es.expression)

    let callExpr = es.expression :?> Ast.CallExpression
    
    testIdentifier callExpr.func exIdent

    Assert.Equal(expectedParameters.Length, callExpr.arguments.Length)

    match num with 
    | 0 -> 
        Assert.Equal(0, callExpr.arguments.Length)
    | 1 ->
        testIntegerLiteral callExpr.arguments.[0] 1L
    | _ ->
        testIntegerLiteral callExpr.arguments.[0] 1L
        testIntInfixExpression callExpr.arguments.[1] 2L "*" 3L
        testIntInfixExpression callExpr.arguments.[2] 4L "+" 5L
        ()

[<Theory>]
[<InlineData("let x = 5;", "x")>]
[<InlineData("let y = true;", "y")>]
[<InlineData("let foobar = y;", "foobar")>]
let ``Can test let statements`` input identifier =
    let program = assertBasicStuff input

    let statement = program.statements.[0]
    
    testLetStatement identifier statement

[<Fact>]
let ``Can test string literal expression`` () =
    let input = "\"hello world\";"

    let program = assertBasicStuff input

    let statement = program.statements.[0]

    testStringLiteral statement  "hello world"

