module EvaluatorTests

open Xunit
open Lexer
open Parser
open Tokens

let canDowncastToInteger (s: Object.Object) =
    match s with 
    | :? Object.Integer as int -> true
    | _ -> false
let canDowncastToBoolean (s: Object.Object) =
    match s with 
    | :? Object.Boolean as int -> true
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

[<Theory>]
[<InlineData("5", 5L)>]
[<InlineData("10", 10L)>]
let ``Can Test and Eval Integers`` input expected =

    let evaluated = testEval input

    testIntegerObject evaluated expected
    

[<Theory>]
[<InlineData("true", true)>]
[<InlineData("false", false)>]
let ``Can Test and Eval Booleans`` input expected =

    let evaluated = testEval input

    testBooleanObject evaluated expected
    
//117