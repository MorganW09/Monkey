module LexerTests

open System
open Xunit
open Tokens
open Lexer

let AssertTokens(lexer: LexerState, expectedToken) =
    let actualToken = nextToken lexer
    Assert.Equal(expectedToken.TokenType, actualToken.TokenType)
    Assert.Equal(expectedToken.Literal, actualToken.Literal)
    ()

let buildTokenTypes(tokens) =
    tokens |> List.map (fun (t, l) -> { TokenType = t ; Literal = l })

[<Fact>]
let ``Can Lex Basic Symbols`` () =
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

[<Fact>]
let ``Can Lex Identifiers`` () =
    let input = "let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);"
    
    let expectedTokensRaw:(TokenType * string) list =
        [
            (TokenType.LET, "let");
            (TokenType.IDENT, "five");
            (TokenType.ASSIGN, "=");
            (TokenType.INT, "5");
            (TokenType.SEMICOLON, ";");
            (TokenType.LET, "let");
            (TokenType.IDENT, "ten");
            (TokenType.ASSIGN, "=");
            (TokenType.INT, "10");
            (TokenType.SEMICOLON, ";");
            (TokenType.LET, "let");
            (TokenType.IDENT, "add");
            (TokenType.ASSIGN, "=");
            (TokenType.FUNCTION, "fn");
            (TokenType.LPAREN, "(");
            (TokenType.IDENT, "x");
            (TokenType.COMMA, ",");
            (TokenType.IDENT, "y");
            (TokenType.RPAREN, ")");
            (TokenType.LBRACE, "{");
            (TokenType.IDENT, "x");
            (TokenType.PLUS, "+");
            (TokenType.IDENT, "y");
            (TokenType.SEMICOLON, ";");
            (TokenType.RBRACE, "}");
            (TokenType.SEMICOLON, ";");
            (TokenType.LET, "let");
            (TokenType.IDENT, "result");
            (TokenType.ASSIGN, "=");
            (TokenType.IDENT, "add");
            (TokenType.LPAREN, "(");
            (TokenType.IDENT, "five");
            (TokenType.COMMA, ",");
            (TokenType.IDENT, "ten");
            (TokenType.RPAREN, ")");
            (TokenType.SEMICOLON, ";");
            (TokenType.EOF, "");
        ]

    let expectedTokens = buildTokenTypes expectedTokensRaw
    let lexer = createLexer input
    expectedTokens |> List.iter (fun et -> AssertTokens(lexer, et))

[<Fact>]
let ``Can Lex More Tokens`` () =
    let input = "!-/*5;

5 < 10 > 5;"
    
    let expectedTokensRaw:(TokenType * string) list =
        [
            (TokenType.BANG, "!");
            (TokenType.MINUS, "-");
            (TokenType.SLASH, "/");
            (TokenType.ASTERISK, "*");
            (TokenType.INT, "5");
            (TokenType.SEMICOLON, ";");
            (TokenType.INT, "5");
            (TokenType.LT, "<");
            (TokenType.INT, "10");
            (TokenType.GT, ">");
            (TokenType.INT, "5");
            (TokenType.SEMICOLON, ";");
            (TokenType.EOF, "");
        ]

    let expectedTokens = buildTokenTypes expectedTokensRaw
    let lexer = createLexer input
    expectedTokens |> List.iter (fun et -> AssertTokens(lexer, et))

[<Fact>]
let ``Can Lex more keywords`` () =
    let input = "if (5 < 10) {
        return true;
    } else {
        return false;
    }"

    let expectedTokensRaw:(TokenType * string) list =
        [

            (TokenType.IF, "if");
            (TokenType.LPAREN, "(");
            (TokenType.INT, "5");
            (TokenType.LT, "<");
            (TokenType.INT, "10");
            (TokenType.RPAREN, ")");
            (TokenType.LBRACE, "{");
            (TokenType.RETURN, "return");
            (TokenType.TRUE, "true");
            (TokenType.SEMICOLON, ";");
            (TokenType.RBRACE, "}");
            (TokenType.ELSE, "else");
            (TokenType.LBRACE, "{");
            (TokenType.RETURN, "return");
            (TokenType.FALSE, "false");
            (TokenType.SEMICOLON, ";");
            (TokenType.RBRACE, "}");
            (TokenType.EOF, "");
        ]

    let expectedTokens = buildTokenTypes expectedTokensRaw
    let lexer = createLexer input
    expectedTokens |> List.iter (fun et -> AssertTokens(lexer, et))
    
[<Fact>]
let ``Can Lex advanced equals/not equals operators`` () =
    let input = "10 == 10
10 != 9"

    let expectedTokensRaw:(TokenType * string) list =
        [
            (TokenType.INT, "10");
            (TokenType.EQ, "==");
            (TokenType.INT, "10");
            (TokenType.INT, "10");
            (TokenType.NOT_EQ, "!=");
            (TokenType.INT, "9");
            (TokenType.EOF, "");
        ]

    let expectedTokens = buildTokenTypes expectedTokensRaw
    let lexer = createLexer input
    expectedTokens |> List.iter (fun et -> AssertTokens(lexer, et))

[<Fact>]
let ``Can lex strings`` () =
    let input = "\"foobar\"
    \"foo bar\""

    let expectedTokensRaw:(TokenType * string) list =
        [
            (TokenType.STRING, "foobar");
            (TokenType.STRING, "foo bar");
            (TokenType.EOF, "");
        ]

    let expectedTokens = buildTokenTypes expectedTokensRaw
    let lexer = createLexer input
    expectedTokens |> List.iter (fun et -> AssertTokens(lexer, et))

[<Fact>]
let ``Can lex array`` () =
    let input = "[1, 2];"

    let expectedTokensRaw:(TokenType * string) list =
        [
            (TokenType.LBRACKET, "[");
            (TokenType.INT, "1");
            (TokenType.COMMA, ",");
            (TokenType.INT, "2");
            (TokenType.RBRACKET, "]");
            (TokenType.SEMICOLON, ";");
            (TokenType.EOF, "");
        ]

    let expectedTokens = buildTokenTypes expectedTokensRaw
    let lexer = createLexer input
    expectedTokens |> List.iter (fun et -> AssertTokens(lexer, et))
[<Fact>]
let ``Can lex hash`` () =
    let input = "{ 1: \"23\"};"

    let expectedTokensRaw:(TokenType * string) list =
        [
            (TokenType.LBRACE, "{");
            (TokenType.INT, "1");
            (TokenType.COLON, ":");
            (TokenType.STRING, "23");
            (TokenType.RBRACE, "}");
            (TokenType.SEMICOLON, ";");
            (TokenType.EOF, "");
        ]

    let expectedTokens = buildTokenTypes expectedTokensRaw
    let lexer = createLexer input
    expectedTokens |> List.iter (fun et -> AssertTokens(lexer, et))