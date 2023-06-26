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

let testEval input =
    let lexer = createLexer input
    let parser = createParser lexer
    let program = parseProgram parser

    Evaluator.eval program

let testIntegerObject (obj: Object.Object option) expected =
    Assert.True(obj.IsSome)

    let someObj = obj.Value
    Assert.True(canDowncastToInteger someObj)

    let int = someObj :?> Object.Integer

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