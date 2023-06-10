module AstTests

open Xunit
open Lexer
open Parser
open Tokens

[<Fact>]
let ``Can Test String`` () =
    let letToken = { TokenType = TokenType.LET; Literal = "let" }
    let myVarToken = { TokenType = TokenType.IDENT; Literal = "myVar" }
    let anotherVarToken = { TokenType = TokenType.IDENT; Literal = "anotherVar" }

    let nameIdentifier = new Ast.Identifier(myVarToken, "myVar")
    let valueIdentifier = new Ast.Identifier(anotherVarToken, "anotherVar")

    let letStatement = new Ast.LetStatement(letToken, nameIdentifier, valueIdentifier)
    
    let program = new Ast.Program([| letStatement |])

    Assert.Equal("let myVar = anotherVar;", (program :> Ast.Node).Str())