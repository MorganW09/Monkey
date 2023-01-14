module Tests

open System
open Xunit
open Tokens
open Lexer

let AssertTokens(lexer: LexerState, expectedToken) =
    let actualToken = Monkey.nextToken lexer
    Assert.Equal(expectedToken.TokenType, actualToken.TokenType)
    Assert.Equal(expectedToken.Literal, actualToken.Literal)
    ()

[<Fact>]
let ``Can Lex Symbols`` () =
    let expectedTokens =
        [
            { TokenType = TokenType.ASSIGN , "=" };
            { TokenType = TokenType.PLUS , "+" };
            { TokenType = TokenType.LPAREN , "(" };
            { TokenType = TokenType.RPAREN , ")" };
            { TokenType = TokenType.LBRACE , "{" };
            { TokenType = TokenType.RBRACE , "}" };
            { TokenType = TokenType.COMMA , "," };
            { TokenType = TokenType.SEMICOLON , ";" };
            { TokenType = TokenType.EOF , "" };
        ]
    
    let input = "=+(){},;"
    let lexer = Monkey.createLexer input
    expectedTokens |> List.iter (fun et -> AssertTokens(lexer, et))
