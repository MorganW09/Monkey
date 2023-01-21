module Tests

open System
open Xunit
open Tokens
open Lexer

let AssertTokens(lexer: LexerState, expectedToken) =
    let actualToken = nextToken lexer
    Assert.Equal(expectedToken.TokenType, actualToken.TokenType)
    Assert.Equal(expectedToken.Literal, actualToken.Literal)
    ()

[<Fact>]
let ``Can Lex Symbols`` () =
    let expectedTokens =
        [
            { TokenType = TokenType.ASSIGN ; Literal = "=" };
            { TokenType = TokenType.PLUS ; Literal = "+" };
            { TokenType = TokenType.LPAREN ; Literal = "(" };
            { TokenType = TokenType.RPAREN ; Literal = ")" };
            { TokenType = TokenType.LBRACE ; Literal = "{" };
            { TokenType = TokenType.RBRACE ; Literal = "}" };
            { TokenType = TokenType.COMMA ; Literal = "," };
            { TokenType = TokenType.SEMICOLON ; Literal = ";" };
            { TokenType = TokenType.EOF ; Literal = "" };
        ]
    
    let input = "=+(){},;"
    let lexer = createLexer input
    expectedTokens |> List.iter (fun et -> AssertTokens(lexer, et))
