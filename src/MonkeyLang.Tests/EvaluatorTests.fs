module EvaluatorTests

open Xunit
open Lexer
open Parser
open System

let canDowncastToInteger (s: Object.Object) =
    match s with 
    | :? Object.Integer as int -> true
    | _ -> false
let canDowncastToBoolean (s: Object.Object) =
    match s with 
    | :? Object.Boolean as int -> true
    | _ -> false
let canDowncastToNull (s: Object.Object) =
    match s with 
    | :? Object.Null as int -> true
    | _ -> false
let canDowncastToReturn (obj : Object.Object) =
    match obj with 
    | :? Object.Return as rtr -> true
    | _ -> false
let canDowncastToError (obj : Object.Object) =
    match obj with 
    | :? Object.Error as err -> true
    | _ -> false
let canDowncastToFunction (obj : Object.Object) =
    match obj with 
    | :? Object.Function as fnc -> true
    | _ -> false
let canDowncastToStr (obj : Object.Object) =
    match obj with 
    | :? Object.Str as str -> true
    | _ -> false

 
let testEval input =
    let lexer = createLexer input
    let parser = createParser lexer
    let program = parseProgram parser

    Evaluator.evaluate program

let testIntegerObject (obj: Object.Object option) expected =
    Assert.True(obj.IsSome, "IsNone")

    let someObj = obj.Value
    let objType = someObj.GetType().ToString()

    Assert.True(canDowncastToInteger someObj, (sprintf "Cannot downcast %s to Integer" objType))

    let int = someObj :?> Object.Integer

    Assert.Equal(expected, int.value)

let testStringLiteral (obj: Object.Object option) expected =
    Assert.True(obj.IsSome, "IsNone")

    let someObj = obj.Value
    let objType = someObj.GetType().ToString()

    Assert.True(canDowncastToStr someObj, (sprintf "Cannot downcast %s to Str" objType))

    let int = someObj :?> Object.Str

    Assert.Equal(expected, int.value)


let testBooleanObject (obj: Object.Object option) expected =
    Assert.True(obj.IsSome, "IsNone")

    let someObj = obj.Value

    Assert.True(canDowncastToBoolean someObj, "Cannot downcast")

    let bool = someObj :?> Object.Boolean

    let msg = someObj.Inspect()

    Assert.True((expected = bool.value), msg)

let testNullObject (obj: Object.Object option) =
    Assert.True(obj.IsSome, "IsNone")

    let someObj = obj.Value
    Assert.True(canDowncastToNull someObj)

let testReturnValue (obj: Object.Object option) expected =
    Assert.True(obj.IsSome, "IsNone")

    let someObj = obj.Value

    Assert.True(canDowncastToReturn someObj, "Cannot downcast to return")

    let rtr = someObj :?> Object.Return

    Assert.True(canDowncastToInteger rtr.value, "Cannot downcast to integer")

    let int = rtr.value :?> Object.Integer

    Assert.Equal(expected, int.value)

let testErrorObject (obj: Object.Object option) expectedError =
    Assert.True(obj.IsSome, "IsNone")

    let someObj = obj.Value

    let objType = someObj.GetType().ToString()

    Assert.True(canDowncastToError someObj, sprintf "Cannot downcast %s to error" objType)

    let error = someObj :?> Object.Error

    Assert.Equal(expectedError, error.message)

[<Theory>]
[<InlineData("5", 5L)>]
[<InlineData("10", 10L)>]
[<InlineData("-5", -5L)>]
[<InlineData("-10", -10L)>]
[<InlineData("5 + 5 + 5 + 5 - 10", 10)>]
[<InlineData("2 * 2 * 2 * 2 * 2", 32)>]
[<InlineData("-50 + 100 + -50", 0)>]
[<InlineData("5 * 2 + 10", 20)>]
[<InlineData("5 + 2 * 10", 25)>]
[<InlineData("20 + 2 * -10", 0)>]
[<InlineData("50 / 2 * 2 + 10", 60)>]
[<InlineData("2 * (5 + 10)", 30)>]
[<InlineData("3 * 3 * 3 + 10", 37)>]
[<InlineData("3 * (3 * 3) + 10", 37)>]
[<InlineData("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50)>]

let ``Can Test and Eval Integers`` input expected =

    let evaluated = testEval input

    testIntegerObject evaluated expected
    

[<Theory>]
[<InlineData("true", true)>]
[<InlineData("false", false)>]
[<InlineData("1 < 2", true)>]
[<InlineData("1 > 2", false)>]
[<InlineData("1 < 1", false)>]
[<InlineData("1 > 1", false)>]
[<InlineData("1 == 1", true)>]
[<InlineData("1 != 1", false)>]
[<InlineData("1 == 2", false)>]
[<InlineData("1 != 2", true)>]
[<InlineData("true == true", true)>]
[<InlineData("false == false", true)>]
[<InlineData("true == false", false)>]
[<InlineData("true != false", true)>]
[<InlineData("false != true", true)>]
[<InlineData("(1 < 2) == true", true)>]
[<InlineData("(1 < 2) == false", false)>]
[<InlineData("(1 > 2) == true", false)>]
[<InlineData("(1 > 2) == false", true)>]
let ``Can Test and Eval Booleans`` input expected =

    let evaluated = testEval input

    testBooleanObject evaluated expected
    
[<Theory>]
[<InlineData("!true", false)>]
[<InlineData("!false", true)>]
[<InlineData("!5", false)>]
[<InlineData("!!true", true)>]
[<InlineData("!!false", false)>]
[<InlineData("!!5", true)>]
let ``Can test bang operator`` input expected =
    let evaluated = testEval input

    testBooleanObject evaluated expected

[<Theory>]
[<InlineData("if (true) { 10 }", 10L)>]
[<InlineData("if (false) { 10 }", null)>]
[<InlineData("if (1) { 10 }", 10L)>]
[<InlineData("if (1 < 2) { 10 }", 10L)>]
[<InlineData("if (1 > 2) { 10 }", null)>]
[<InlineData("if (1 > 2) { 10 } else { 20 }", 20L)>]
[<InlineData("if (1 < 2) { 10 } else { 20 }", 10L)>]
let ``Can test if else expressions`` input (expected: int64 Nullable ) =
    let evaluated = testEval input

    match expected.HasValue with 
    | true ->
        testIntegerObject evaluated expected.Value
    | false -> 
        testNullObject evaluated

[<Theory>]
[<InlineData("return 10;", 10L)>]
[<InlineData("return 10; 9;", 10L)>]
[<InlineData("return 2 * 5; 9;", 10L)>]
[<InlineData("9; return 2 * 5; 9;", 10L)>]
[<InlineData("if (10 > 1) {
if (10 > 1) {
return 10;
}
return 1;
}", 10L)>]
let ``Can test evaluation of return statements`` input (expected: int64) =
    let evaluated = testEval input

    testIntegerObject evaluated expected

[<Theory>]
[<InlineData("5 + true;","type mismatch: INTEGER + BOOLEAN")>]
[<InlineData("5 + true; 5;","type mismatch: INTEGER + BOOLEAN")>]
[<InlineData("-true","unknown operator: -BOOLEAN")>]
[<InlineData("true + false;","unknown operator: BOOLEAN + BOOLEAN")>]
[<InlineData("\"hello\" - \"world\";","unknown operator: STRING - STRING")>]
[<InlineData("5; true + false; 5","unknown operator: BOOLEAN + BOOLEAN")>]
[<InlineData("if (10 > 1) { true + false; }","unknown operator: BOOLEAN + BOOLEAN")>]
[<InlineData("if (10 > 1) { if (10 > 1) { return true + false; } return 1; }","unknown operator: BOOLEAN + BOOLEAN")>]
[<InlineData("foobar", "identifier not found: foobar")>]
let ``Can test error handling`` input expectedError =
    let evaluated = testEval input

    testErrorObject evaluated expectedError

[<Theory>]
[<InlineData("let a = 5; a;", 5L)>]
[<InlineData("let a = 5 * 5; a;", 25L)>]
[<InlineData("let a = 5; let b = a; b;", 5L)>]
[<InlineData("let a = 5; let b = a; let c = a + b + 5; c;", 15L)>]
let ``Can test let statements`` input expected =
    let evaluated = testEval input

    testIntegerObject evaluated expected

[<Fact>]
let ``Can test function object`` () =
    let input = "fn(x) { x + 2; };"

    let evaluated = testEval input

    Assert.True(evaluated.IsSome, "IsNone")

    let someObj = evaluated.Value

    Assert.True(canDowncastToFunction someObj, "Cannot downcast to function")

    let func = someObj :?> Object.Function

    Assert.Equal(1, func.parameters.Length)

    let param = (func.parameters.[0] :> Ast.Expression).Str()
    Assert.Equal("x", param)

    let bodyStr = (func.body :> Ast.Statement).Str()

    Assert.Equal("(x + 2)", bodyStr)

[<Theory>]
[<InlineData("let identity = fn(x) { x; }; identity(5);", 5L)>]
[<InlineData("let identity = fn(x) { return x; }; identity(5);", 5L)>]
[<InlineData("let double = fn(x) { x * 2; }; double(5);", 10L)>]
[<InlineData("let add = fn(x, y) { x + y; }; add(5, 5);", 10L)>]
[<InlineData("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20L)>]
[<InlineData("fn(x) { x; }(5)", 5L)>]
let ``CanTestFunctionApplications`` input expected =
    let evaluated = testEval input

    testIntegerObject evaluated expected

[<Fact>]
let ``Can test closures`` () =
    let input = "let newAdder = fn(x) {
fn(y) { x + y };
};
let addTwo = newAdder(2);
addTwo(2);"

    let evaluated = testEval input

    testIntegerObject evaluated 4L

[<Fact>]
let ``Can test string literal`` () =
    let input = "\"Hello world!\""

    let evaluated = testEval input

    testStringLiteral evaluated "Hello world!"

[<Fact>]
let ``Can test string concatenation`` () =
    let input = "\"Hello\" + \" \" + \"world!\""

    let evaluated = testEval input

    testStringLiteral evaluated "Hello world!"

[<Theory>]
[<InlineData("len(\"\")", 0L)>]
[<InlineData("len(\"four\")", 4L)>]
[<InlineData("len(\"hello world\")", 11L)>]
let ``Can test len built in function success`` input expected =
    let evaluated = testEval input

    testIntegerObject evaluated expected

[<Theory>]
[<InlineData("len(1)", "argument to \"len\" not supported, got INTEGER")>]
[<InlineData("len(\"one\", \"two\")", "wrong number of arguments. got=2, want=1")>]
let ``Can test len built in function errors`` input expected =
    let evaluated = testEval input

    testErrorObject evaluated expected

//167