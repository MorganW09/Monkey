module Parser
    open Tokens
    type ParserState = 
        {
            lexer : Lexer.LexerState
            mutable curToken : Tokens.Token
            mutable peekToken : Tokens.Token
        }

    let nextToken (p: ParserState) =
        p.curToken <- p.peekToken
        p.peekToken <- Lexer.nextToken p.lexer

    let curTokenIs (p: ParserState) (t: TokenType) =
        p.curToken.TokenType = t
    
    let peekTokenIs (p: ParserState) (t: TokenType) =
        p.peekToken.TokenType = t

    let expectPeek (p: ParserState) (t: TokenType) =
        match peekTokenIs p t with
        | true -> 
            nextToken p 
            true
        | false -> false

    let parseLetStatement (p: ParserState) =
        let letToken = p.curToken

        //enforce this with something
        expectPeek p TokenType.IDENT |> ignore

        let identStatement = new Ast.Identifier(p.curToken, p.curToken.Literal)

        //enforce this with something
        expectPeek p TokenType.ASSIGN |> ignore

        //TODO - actually generate expression for value
        let mutable notSemicolon = true
        while notSemicolon do
            nextToken p
            notSemicolon <- not(curTokenIs p TokenType.SEMICOLON)

        //TODO - stop gap solution until we can create an actual INT expression
        let blankExpression = new Ast.Identifier(p.curToken, "blank")

        new Ast.LetStatement(letToken, identStatement, blankExpression)

    let parseStatement (p: ParserState) =
        match p.curToken.TokenType with 
        | TokenType.LET -> parseLetStatement p
        | _ -> parseLetStatement p

    let createParser lexer =
        let firstToken = Lexer.nextToken lexer
        let secondToken = Lexer.nextToken lexer
        let parser = { lexer = lexer; curToken = firstToken; peekToken = secondToken }
        parser

    let parseProgram (parser: ParserState) : Ast.Program =
        let list = new ResizeArray<Ast.Statement>()

        while not (curTokenIs parser TokenType.EOF) do
            let statement = parseStatement parser

            list.Add(statement)

            nextToken parser

        let statementArray = list.ToArray()
        { Statements = statementArray }




