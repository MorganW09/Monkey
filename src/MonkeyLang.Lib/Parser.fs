module Parser
    open Tokens

    type ExprPrecedence =
    | LOWEST = 1
    | EQUALS = 2
    | LESSGREATER = 3
    | SUM = 4
    | PRODUCT = 5
    | PREFIX = 6
    | CALL = 7

    type prefixParse = ParserState -> Ast.Expression
    and infixParse = Ast.Expression -> Ast.Expression
    and ParserState = 
        {
            lexer : Lexer.LexerState
            mutable curToken : Tokens.Token
            mutable peekToken : Tokens.Token
            mutable errors : ResizeArray<string> //TODO - maybe change to option type??
            prefixParseFns : System.Collections.Generic.Dictionary<TokenType, prefixParse>
            infixParseFns : System.Collections.Generic.Dictionary<TokenType, infixParse>
        }

    let registerPrefix p token func =
        match p.prefixParseFns.ContainsKey token with
        | true -> ()
        | false -> p.prefixParseFns.Add(token, func)

    let nextToken (p: ParserState) =
        p.curToken <- p.peekToken
        p.peekToken <- Lexer.nextToken p.lexer

    let curTokenIs (p: ParserState) (t: TokenType) =
        p.curToken.TokenType = t
    
    let peekTokenIs (p: ParserState) (t: TokenType) =
        p.peekToken.TokenType = t

    let peekError (p: ParserState) (t: TokenType) =
        let msg = sprintf "expected next token to be %s, got %s instead" (t.ToString()) (p.peekToken.TokenType.ToString())
        p.errors.Add(msg)

    let expectPeek (p: ParserState) (t: TokenType) =
        match peekTokenIs p t with
        | true -> 
            nextToken p 
            true
        | false -> 
            peekError p t
            false

    let parseIdentifier  p =
        (new Ast.Identifier (p.curToken, p.curToken.Literal)) :> Ast.Expression

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

    let parseReturnStatement p =
        let returnToken = p.curToken

        nextToken p

        //TODO - actually generate expression for value
        let mutable notSemicolon = true
        while notSemicolon do
            nextToken p
            notSemicolon <- not(curTokenIs p TokenType.SEMICOLON)

        //TODO - stop gap solution until we can create an actual INT expression
        let blankExpression = new Ast.Identifier(p.curToken, "blank")

        new Ast.ReturnStatement(returnToken, blankExpression)

    let parseExpression p precedence =
        // match p.prefixParseFns.ContainsKey p.curToken.TokenType with
        // | true -> 
        //     let prefix = p.prefixParseFns[p.curToken.TokenType]
        //     let leftExp = prefix()
        //     Some leftExp
        // | false -> None
        //do this safely
        let prefix = p.prefixParseFns.[p.curToken.TokenType]
        prefix p

    let parseIntegerLiteral p =
        //more stuff for integer literal

    let parseExpressionStatement p =
        let curToken = p.curToken
        let expression = parseExpression p ExprPrecedence.LOWEST

        let statement = new Ast.ExpressionStatement (curToken, expression)

        if peekTokenIs p TokenType.SEMICOLON then nextToken p

        statement

    let parseStatement (p: ParserState) : Ast.Statement =
        match p.curToken.TokenType with 
        | TokenType.LET -> (parseLetStatement p :> Ast.Statement)
        | TokenType.RETURN -> (parseReturnStatement p :> Ast.Statement)
        | _ -> 
            //TODO - figure something else out
            (parseExpressionStatement p :> Ast.Statement)

    let createParser lexer =
        let firstToken = Lexer.nextToken lexer
        let secondToken = Lexer.nextToken lexer

        //register parse functions
        let prefixFns = new System.Collections.Generic.Dictionary<TokenType, prefixParse>()
        prefixFns.Add(TokenType.IDENT, parseIdentifier)

        let parser = 
            { 
                lexer = lexer
                curToken = firstToken
                peekToken = secondToken
                errors = new ResizeArray<string>()
                prefixParseFns = prefixFns
                infixParseFns = new System.Collections.Generic.Dictionary<TokenType, infixParse>()
            }

        parser

    let parseProgram (parser: ParserState) : Ast.Program =
        let list = new ResizeArray<Ast.Statement>()

        while not (curTokenIs parser TokenType.EOF) do
            let statement = parseStatement parser

            list.Add(statement)

            nextToken parser

        let statementArray = list.ToArray()

        new Ast.Program(statementArray)




