module Tests

open System
open Xunit
open Tokens
open Lexer

let AssertTokens(lexer: LexerState, expectedToken) =
    let actualToken = Lexer.nextToken lexer
    Assert.Equal(expectedToken.TokenType, actualToken.TokenType)
    Assert.Equal(expectedToken.Literal, actualToken.Literal)



[<Fact>]
let ``LookupIdent fn test`` () =
    let fn = Lexer.lookupIdent "fn"
    Assert.Equal(Function, fn)

[<Fact>]
let ``LookupIdent let test `` () =
    let lt = Lexer.lookupIdent "let"
    Assert.Equal(Let, lt)

[<Fact>]
let ``LookupIdent other test`` () =
    let ident = Lexer.lookupIdent "ls"
    Assert.Equal(Ident, ident)
    
[<Fact>]
let ``LookupNumber test`` () =
    let num = Lexer.lookupNumber "2"
    Assert.Equal(Int, num)
    
[<Theory>]
[<InlineData('`', false)>]
[<InlineData('a', true)>]
[<InlineData('g', true)>]
[<InlineData('z', true)>]
[<InlineData('{', false)>]
[<InlineData('@', false)>]
[<InlineData('A', true)>]
[<InlineData('M', true)>]
[<InlineData('A', true)>]
[<InlineData('[', false)>]
[<InlineData('^', false)>]
[<InlineData('_', true)>]
[<InlineData(' ', false)>]
let ``IsLetter correctly identifies letters`` char expectedResult =
    Assert.Equal(expectedResult, Lexer.isLetter char)
        
[<Theory>]
[<InlineData('`', false)>]
[<InlineData('a', false)>]
[<InlineData('g', false)>]
[<InlineData('z', false)>]
[<InlineData('{', false)>]
[<InlineData('@', false)>]
[<InlineData('A', false)>]
[<InlineData('M', false)>]
[<InlineData('A', false)>]
[<InlineData('[', false)>]
[<InlineData('^', false)>]
[<InlineData('_', false)>]
[<InlineData(' ', false)>]
[<InlineData('/', false)>]
[<InlineData('0', true)>]
[<InlineData('5', true)>]
[<InlineData('9', true)>]
[<InlineData(':', false)>]
let ``IsDigit correctly identifies digits`` char expectedResult =
    Assert.Equal(expectedResult, Lexer.isDigit char)
    
[<Theory>]
[<InlineData("aa", true)>]
[<InlineData("a1", false)>]
[<InlineData("a{", false)>]
[<InlineData("aA", true)>]
let ``CanReadLetter correctly determines when to stop reading current token`` input expected =
    let lexer = createLexer input
    let can = Lexer.canReadNextLetter lexer
    Assert.Equal(expected, can)
        
[<Theory>]
[<InlineData("10", true)>]
[<InlineData("1a", false)>]
[<InlineData("1.", false)>]
[<InlineData("10", true)>]
let ``CanReadDigit correctly determines when to stop reading current token`` input expected =
    let lexer = createLexer input
    let can = Lexer.canReadNextDigit lexer
    Assert.Equal(expected, can)

[<Theory>]
[<InlineData("a", false)>]
[<InlineData("aa", true)>]
let ``CanRead doesn't error at end of input`` input expected =
    let lexer = createLexer input
    let can = Lexer.canReadNextLetter lexer
    Assert.Equal(expected, can)

[<Theory>]
[<InlineData("123", '2')>]
[<InlineData("abc", 'b')>]
let ``readChar only reads one char at a time`` input expected =
    let lexer = createLexer input
    Lexer.readChar lexer |> ignore
    Assert.Equal(expected, lexer.ch)

[<Theory>]
[<InlineData("12")>]
[<InlineData("ab")>]
[<InlineData("1ab")>]
[<InlineData("sfs")>]
let ``readChar sets ch to null when read past end of input`` input =
    let lexer = createLexer input
    Lexer.nextToken lexer |> ignore
    Lexer.nextToken lexer |> ignore
    Lexer.nextToken lexer |> ignore
    Lexer.nextToken lexer |> ignore
    Assert.Equal('\000', lexer.ch)

[<Theory>]
[<InlineData("5463 five", "5463")>]
[<InlineData("546323five", "546323")>]
let ``readNumber reads next number`` input expected =
    let lexer = createLexer input
    let digitToken = Lexer.readNumber lexer
    Assert.Equal(expected, digitToken.Literal)
    
[<Fact>]
let ``readIdentifier reads next let identifier`` () =
    let lexer = createLexer "let 10"
    let identToken = Lexer.readIdentifier lexer
    Assert.Equal("let", identToken.Literal)
    Assert.Equal(Let, identToken.TokenType)
    
[<Fact>]
let ``readIdentifier reads next fn identifier`` () =
    let lexer = createLexer "fn (a"
    let identToken = Lexer.readIdentifier lexer
    Assert.Equal("fn", identToken.Literal)
    Assert.Equal(Function, identToken.TokenType)
    
[<Fact>]
let ``readIdentifier reads next ident identifier`` () =
    let lexer = createLexer "ten = 10"
    let identToken = Lexer.readIdentifier lexer
    Assert.Equal("ten", identToken.Literal)
    Assert.Equal(Ident, identToken.TokenType)
    
[<Fact>]
let ``determineComplexTokenType identifies letter token`` () =
    let lexer = createLexer "let 10 = z"
    let tokenType = Lexer.determineComplexTokenType lexer
    Assert.Equal(Letter, tokenType)
    
[<Fact>]
let ``determineComplexTokenType identifies digit token`` () =
    let lexer = createLexer "10 = z"
    let tokenType = Lexer.determineComplexTokenType lexer
    Assert.Equal(Digit, tokenType)
    
[<Fact>]
let ``determineComplexTokenType identifies illegal token`` () =
    let lexer = createLexer "? 10 = z"
    let tokenType = Lexer.determineComplexTokenType lexer
    Assert.Equal(Illegal, tokenType)

[<Fact>]
let ``nextComplexToken reads next function`` () =
    let lexer = createLexer "fn (a"
    let complexToken = Lexer.nextComplexToken lexer
    Assert.Equal("fn", complexToken.Literal)
    Assert.Equal(Function, complexToken.TokenType)
    
[<Fact>]
let ``nextComplexToken reads next let identifier`` () =
    let lexer = createLexer "let (a"
    let identToken = Lexer.nextComplexToken lexer
    Assert.Equal("let", identToken.Literal)
    Assert.Equal(Let, identToken.TokenType)
    
[<Fact>]
let ``nextComplexToken reads next ident identifier`` () =
    let lexer = createLexer "ten = 10"
    let identToken = Lexer.nextComplexToken lexer
    Assert.Equal("ten", identToken.Literal)
    Assert.Equal(Ident, identToken.TokenType)
    
[<Fact>]
let ``nextComplexToken reads next number`` () =
    let lexer = createLexer "10 = 10"
    let identToken = Lexer.nextComplexToken lexer
    Assert.Equal("10", identToken.Literal)
    Assert.Equal(Int, identToken.TokenType)
    
[<Fact>]
let ``nextComplexToken returns illegal token`` () =
    let lexer = createLexer "? = 10"
    let identToken = Lexer.nextComplexToken lexer
    Assert.Equal("?", identToken.Literal)
    Assert.Equal(TokenType.Illegal, identToken.TokenType)

[<Fact>]
let ``skipWhitespaces skips whitespace`` () =
    let lexer = createLexer "      ("
    Lexer.skipWhitespace lexer
    let token = Lexer.nextToken lexer
    Assert.Equal(Lparen, token.TokenType)
    Assert.Equal("(", token.Literal)

[<Theory>]
[<InlineData("let")>]
[<InlineData("10")>]
[<InlineData("ten")>]
[<InlineData("fn")>]
[<InlineData("x")>]
let ``skipWhitespace only skips whitespace`` input =
    let lexer = createLexer input
    Lexer.skipWhitespace lexer
    let token = Lexer.nextToken lexer
    Assert.Equal(input, token.Literal)

[<Theory>]
[<InlineData("==", "==")>]
[<InlineData("=a", "=")>]
[<InlineData("a=", "a")>]
[<InlineData("!=", "!=")>]
[<InlineData("!a", "!")>]
[<InlineData("a!", "a")>]
let ``nextEqualsOperator correctly reads double operators`` input expected =
    let lexer = createLexer input
    let token = Lexer.nextToken lexer
    Assert.Equal(expected, token.Literal)

[<Fact>]
let ``Can Lex Symbols`` () =
    let expectedTokens = 
        [
            { TokenType = Assign; Literal = "=" };
            { TokenType = Plus; Literal = "+" };
            { TokenType = Lparen; Literal = "(" };
            { TokenType = Rparen; Literal = ")" };
            { TokenType = Lbrace; Literal = "{" };
            { TokenType = Rbrace; Literal = "}" };
            { TokenType = Comma; Literal = "," };
            { TokenType = Semicolon; Literal = ";" };
            { TokenType = Eof; Literal = "" }
        ]
    
    let input = "=+(){},;"
    let lexer = Lexer.createLexer input
    expectedTokens |> List.iter (fun et -> AssertTokens(lexer, et))

[<Fact>]
let ``Can Lex Simple Program`` () =
    let input = 
        "let five = 5;
let ten = 10;

let add = fn(x, y) {
    x + y;
};

let result = add(five, ten);"  
    let expectedTokens = [
        { TokenType = Let; Literal = "let" };
        { TokenType = Ident; Literal = "five" };
        { TokenType = Assign; Literal = "=" };
        { TokenType = Int; Literal = "5" };
        { TokenType = Semicolon; Literal = ";" };

        
        { TokenType = Let; Literal = "let" };
        { TokenType = Ident; Literal = "ten" };
        { TokenType = Assign; Literal = "=" };
        { TokenType = Int; Literal = "10" };
        { TokenType = Semicolon; Literal = ";" };

        
        { TokenType = Let; Literal = "let" };
        { TokenType = Ident; Literal = "add" };
        { TokenType = Assign; Literal = "=" };
        { TokenType = Function; Literal = "fn" };
        { TokenType = Lparen; Literal = "(" };
        { TokenType = Ident; Literal = "x" };
        { TokenType = Comma; Literal = "," };
        { TokenType = Ident; Literal = "y" };
        { TokenType = Rparen; Literal = ")" };
        { TokenType = Lbrace; Literal = "{" };
        { TokenType = Ident; Literal = "x" };
        { TokenType = Plus; Literal = "+" };
        { TokenType = Ident; Literal = "y" };
        { TokenType = Semicolon; Literal = ";" };
        { TokenType = Rbrace; Literal = "}" };
        { TokenType = Semicolon; Literal = ";" };

        
        { TokenType = Let; Literal = "let" };
        { TokenType = Ident; Literal = "result" };
        { TokenType = Assign; Literal = "=" };
        { TokenType = Ident; Literal = "add" };
        { TokenType = Lparen; Literal = "(" };
        { TokenType = Ident; Literal = "five" };
        { TokenType = Comma; Literal = "," };
        { TokenType = Ident; Literal = "ten" };
        { TokenType = Rparen; Literal = ")" };
        { TokenType = Semicolon; Literal = ";" };
        { TokenType = Eof; Literal = "" };    
    ]
    
    let lexer = Lexer.createLexer input
    expectedTokens |> List.iter (fun et -> AssertTokens(lexer, et))

[<Fact>]
let ``Can Lex Extended Token Set`` () =
    let input = 
        "!-/*5;
5 < 10 > 5;"  

    let expectedTokens = [
        { TokenType = Bang; Literal = "!"};
        { TokenType = Minus; Literal = "-"};
        { TokenType = Slash; Literal = "/"};
        { TokenType = Asterik; Literal = "*"};
        { TokenType = Int; Literal = "5"};
        { TokenType = Semicolon; Literal = ";"};
        
        { TokenType = Int; Literal = "5"};
        { TokenType = Lt; Literal = "<"};
        { TokenType = Int; Literal = "10"};
        { TokenType = Gt; Literal = ">"};
        { TokenType = Int; Literal = "5"};
        { TokenType = Semicolon; Literal = ";"};
    ]

    let lexer = Lexer.createLexer input
    expectedTokens |> List.iter (fun et -> AssertTokens(lexer, et))

[<Fact>]
let ``Can Lex more control tokens`` () =
    let input =
        "if (5 < 10) {
    return true;
} else {
    return false;
}"
    let expectedTokens = [
        { TokenType = If; Literal = "if"};
        { TokenType = Lparen; Literal = "("};
        { TokenType = Int; Literal = "5"};
        { TokenType = Lt; Literal = "<"};
        { TokenType = Int; Literal = "10"};
        { TokenType = Rparen; Literal = ")"};
        { TokenType = Lbrace; Literal = "{"};

        { TokenType = Return; Literal = "return"};
        { TokenType = True; Literal = "true"};
        { TokenType = Semicolon; Literal = ";"};
        
        { TokenType = Rbrace; Literal = "}"};
        { TokenType = Else; Literal = "else"};
        { TokenType = Lbrace; Literal = "{"};

        { TokenType = Return; Literal = "return"};
        { TokenType = False; Literal = "false"};
        { TokenType = Semicolon; Literal = ";"};
        
        { TokenType = Rbrace; Literal = "}"};        
    ]
    
    let lexer = Lexer.createLexer input
    expectedTokens |> List.iter (fun et -> AssertTokens(lexer, et))

[<Fact>]
let ``Can lex double operators`` () =
    let input =
        "10 == 10;
10 != 9;"

    let expectedTokens = [
        { TokenType = Int; Literal = "10"};
        { TokenType = Eq; Literal = "=="};
        { TokenType = Int; Literal = "10"};
        { TokenType = Semicolon; Literal = ";"};
        { TokenType = Int; Literal = "10"};
        { TokenType = Not_Eq; Literal = "!="};
        { TokenType = Int; Literal = "9"};
        { TokenType = Semicolon; Literal = ";"};
    ]
    
    let lexer = Lexer.createLexer input
    expectedTokens |> List.iter (fun et -> AssertTokens(lexer, et))

[<Fact>]
let ``CreateLexer initializes lexer correctly``() =
    let input = "(){}"
    let lexer = Lexer.createLexer input
    Assert.Equal("(){}", lexer.input)
    Assert.Equal(0, lexer.position)
    Assert.Equal(1, lexer.readPosition)
    Assert.Equal('(', lexer.ch)