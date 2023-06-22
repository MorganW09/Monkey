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

    let PrecedenceMap =
        Map.empty
            .Add(TokenType.EQ, ExprPrecedence.EQUALS)
            .Add(TokenType.NOT_EQ, ExprPrecedence.EQUALS)
            .Add(TokenType.LT, ExprPrecedence.LESSGREATER)
            .Add(TokenType.GT, ExprPrecedence.LESSGREATER)
            .Add(TokenType.PLUS, ExprPrecedence.SUM)
            .Add(TokenType.MINUS, ExprPrecedence.SUM)
            .Add(TokenType.SLASH, ExprPrecedence.PRODUCT)
            .Add(TokenType.ASTERISK, ExprPrecedence.PRODUCT)

    type prefixParse = ParserState -> Ast.Expression option
    and infixParse = ParserState -> Ast.Expression -> Ast.Expression option

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

    let getTokenPrecedence tokenType =
        match PrecedenceMap.ContainsKey tokenType with
        | true ->
            PrecedenceMap.[tokenType]
            //precedence
        | false -> 
            ExprPrecedence.LOWEST

    let peekPrecedence p =
        getTokenPrecedence p.peekToken.TokenType

    let curPrecedence p =
        getTokenPrecedence p.curToken.TokenType

    let expectPeek (p: ParserState) (t: TokenType) =
        match peekTokenIs p t with
        | true -> 
            nextToken p 
            true
        | false -> 
            peekError p t
            false

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
        match p.prefixParseFns.ContainsKey p.curToken.TokenType with
        | true -> 
            let prefix = p.prefixParseFns.[p.curToken.TokenType]
            let mutable leftExp = prefix p

            while not (peekTokenIs p TokenType.SEMICOLON) && precedence < peekPrecedence p do
                
                leftExp <- match p.infixParseFns.ContainsKey p.peekToken.TokenType && leftExp.IsSome with
                            | true ->
                                let infixFn = p.infixParseFns.[p.peekToken.TokenType]
                                nextToken p
                                infixFn p leftExp.Value
                            | _ -> leftExp
            leftExp
        | false -> 
            p.errors.Add("no prefix parse function for ! found")
            None
        //do this safely
        // let prefix = p.prefixParseFns.[p.curToken.TokenType]
        // prefix p

    let parseIdentifier  p =
        let identifier = new Ast.Identifier (p.curToken, p.curToken.Literal)
        let expr = identifier :> Ast.Expression
        Some expr

    let parseIntegerLiteral p =
        //more stuff for integer literal        
        match System.Int64.TryParse p.curToken.Literal with
        | true, l ->
            let intLiteral = new Ast.IntegerLiteral(p.curToken, l)
            let expr = intLiteral :> Ast.Expression
            Some expr
        | _ -> 
            let errorMsg = sprintf "could not parse %s as integer" p.curToken.Literal
            p.errors.Add(errorMsg)
            None

    let parseBoolean p =
        let boolean = new Ast.Boolean (p.curToken, curTokenIs p TokenType.TRUE)
        Some (boolean :> Ast.Expression)
    
    let parsePrefixExpression p =
        let curToken = p.curToken

        nextToken p

        match parseExpression p ExprPrecedence.PREFIX with
        | Some expr ->
            let prefix = new Ast.PrefixExpression(curToken, curToken.Literal, expr)
            Some (prefix :> Ast.Expression)
        | None -> None

    let parseInfixExpression p left =
        let curToken = p.curToken

        let precedence = curPrecedence p

        nextToken p

        match parseExpression p precedence with
        | Some right ->
            let infix = new Ast.InfixExpression(curToken, left, curToken.Literal, right)
            Some (infix :> Ast.Expression)
        | None -> None

    let parseExpressionStatement p =
        let curToken = p.curToken
        
        match parseExpression p ExprPrecedence.LOWEST with
        | Some expression ->
            let statement = new Ast.ExpressionStatement (curToken, expression)

            if peekTokenIs p TokenType.SEMICOLON then nextToken p

            Some (statement :> Ast.Statement)
        | None -> None

    let parseGroupedExpression p =
        nextToken p

        match parseExpression p ExprPrecedence.LOWEST with
        | Some expression ->
            if not (expectPeek p TokenType.RPAREN) then None
            else Some expression
        | None -> None

    let parseStatement (p: ParserState) =
        match p.curToken.TokenType with 
        | TokenType.LET -> Some (parseLetStatement p :> Ast.Statement)
        | TokenType.RETURN -> Some (parseReturnStatement p :> Ast.Statement)
        | _ -> 
            //TODO - figure something else out
            (parseExpressionStatement p)

    let createParser lexer =
        let firstToken = Lexer.nextToken lexer
        let secondToken = Lexer.nextToken lexer

        //register prefix parse functions
        let prefixFns = new System.Collections.Generic.Dictionary<TokenType, prefixParse>()
        prefixFns.Add(TokenType.IDENT, parseIdentifier)
        prefixFns.Add(TokenType.INT, parseIntegerLiteral)
        prefixFns.Add(TokenType.BANG, parsePrefixExpression)
        prefixFns.Add(TokenType.MINUS, parsePrefixExpression)
        prefixFns.Add(TokenType.TRUE, parseBoolean)
        prefixFns.Add(TokenType.FALSE, parseBoolean)
        prefixFns.Add(TokenType.LPAREN, parseGroupedExpression)

        //regist infix parse functions
        let infixFns = new System.Collections.Generic.Dictionary<TokenType, infixParse>()
        infixFns.Add(TokenType.PLUS, parseInfixExpression)
        infixFns.Add(TokenType.MINUS, parseInfixExpression)
        infixFns.Add(TokenType.SLASH, parseInfixExpression)
        infixFns.Add(TokenType.ASTERISK, parseInfixExpression)
        infixFns.Add(TokenType.EQ, parseInfixExpression)
        infixFns.Add(TokenType.NOT_EQ, parseInfixExpression)
        infixFns.Add(TokenType.LT, parseInfixExpression)
        infixFns.Add(TokenType.GT, parseInfixExpression)

        let parser = 
            { 
                lexer = lexer
                curToken = firstToken
                peekToken = secondToken
                errors = new ResizeArray<string>()
                prefixParseFns = prefixFns
                infixParseFns = infixFns
            }

        parser

    let parseProgram (parser: ParserState) : Ast.Program =
        let list = new ResizeArray<Ast.Statement>()

        while not (curTokenIs parser TokenType.EOF) do
            match parseStatement parser with
            | Some statement -> list.Add(statement)
            | None -> ()

            nextToken parser

        let statementArray = list.ToArray()

        new Ast.Program(statementArray)




