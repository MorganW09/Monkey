module Tests

open System
open Xunit
open Tokens
open Monkey

let AssertTokens(lexer: LexerState, expectedToken) =
    let actualToken = Monkey.nextToken lexer
    Assert.Equal(expectedToken.TokenType, actualToken.TokenType)
    Assert.Equal(expectedToken.Literal, actualToken.Literal)
    ()

[<Fact>]
let ``Can Lex Symbols`` () =
    let expectedTokens = 
        [
            { TokenType = TokenType.ASSIGN; Literal = "=" };
            { TokenType = TokenType.PLUS; Literal = "+" };
            { TokenType = TokenType.LPAREN; Literal = "(" };
            { TokenType = TokenType.RPAREN; Literal = ")" };
            { TokenType = TokenType.LBRACE; Literal = "{" };
            { TokenType = TokenType.RBRACE; Literal = "}" };
            { TokenType = TokenType.COMMA; Literal = "," };
            { TokenType = TokenType.SEMICOLON; Literal = ";" };
            { TokenType = TokenType.EOF; Literal = "" }
        ]
    
    let input = "=+(){},;"
    let lexer = Monkey.createLexer input
    expectedTokens |> List.iter (fun et -> AssertTokens(lexer, et))
    ()

[<Fact>]
let ``Can Lex Simple Program`` () =
// [<Fact>]
 //let ``Can Lex Simple Program`` () =
    let input = 
        "let five = 5;
let ten = 10;

let add = fn(x, y) {
    x + y;
};

let result = add(five, ten);"  
    let expectedTokens = [
        { TokenType = TokenType.LET; Literal = "let" };
        { TokenType = TokenType.IDENT; Literal = "five" };
        { TokenType = TokenType.ASSIGN; Literal = "=" };
        { TokenType = TokenType.INT; Literal = "5" };
        { TokenType = TokenType.SEMICOLON; Literal = ";" };

        
        { TokenType = TokenType.LET; Literal = "let" };
        { TokenType = TokenType.IDENT; Literal = "ten" };
        { TokenType = TokenType.ASSIGN; Literal = "=" };
        { TokenType = TokenType.INT; Literal = "10" };
        { TokenType = TokenType.SEMICOLON; Literal = ";" };

        
        { TokenType = TokenType.LET; Literal = "let" };
        { TokenType = TokenType.IDENT; Literal = "add" };
        { TokenType = TokenType.ASSIGN; Literal = "=" };
        { TokenType = TokenType.FUNCTION; Literal = "fn" };
        { TokenType = TokenType.LPAREN; Literal = "(" };
        { TokenType = TokenType.IDENT; Literal = "x" };
        { TokenType = TokenType.COMMA; Literal = "," };
        { TokenType = TokenType.IDENT; Literal = "y" };
        { TokenType = TokenType.RPAREN; Literal = ")" };
        { TokenType = TokenType.LBRACE; Literal = "{" };
        { TokenType = TokenType.IDENT; Literal = "x" };
        { TokenType = TokenType.PLUS; Literal = "+" };
        { TokenType = TokenType.IDENT; Literal = "y" };
        { TokenType = TokenType.SEMICOLON; Literal = ";" };
        { TokenType = TokenType.RBRACE; Literal = "}" };
        { TokenType = TokenType.SEMICOLON; Literal = ";" };

        
        { TokenType = TokenType.LET; Literal = "let" };
        { TokenType = TokenType.IDENT; Literal = "result" };
        { TokenType = TokenType.ASSIGN; Literal = "=" };
        { TokenType = TokenType.IDENT; Literal = "add" };
        { TokenType = TokenType.LPAREN; Literal = "(" };
        { TokenType = TokenType.IDENT; Literal = "five" };
        { TokenType = TokenType.COMMA; Literal = "," };
        { TokenType = TokenType.IDENT; Literal = "ten" };
        { TokenType = TokenType.RPAREN; Literal = ")" };
        { TokenType = TokenType.SEMICOLON; Literal = ";" };
        { TokenType = TokenType.EOF; Literal = "" };    
    ]
    
    let lexer = Monkey.createLexer input
    for expectedToken in expectedTokens do
        let token = Monkey.nextToken lexer
        Assert.Equal(expectedToken.TokenType, token.TokenType)
        Assert.Equal(expectedToken.Literal, token.Literal)
    ()