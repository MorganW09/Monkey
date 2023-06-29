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
    | INDEX = 8

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
            .Add(TokenType.LPAREN, ExprPrecedence.CALL)
            .Add(TokenType.LBRACKET, ExprPrecedence.INDEX)

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

    let toExpr exp = exp :> Ast.Expression

    let toSome (exp: Ast.Expression) = Some exp

    let toSomeExpr exp =
        exp
        |> toExpr
        |> toSome

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
            p.errors.Add(sprintf "no prefix parse function for %s found" p.curToken.Literal)
            None

    let parseLetStatement (p: ParserState) =
        let letToken = p.curToken

        if not (expectPeek p TokenType.IDENT) then
            None
        else
            let identStatement = new Ast.Identifier(p.curToken, p.curToken.Literal)

            if not (expectPeek p TokenType.ASSIGN) then
                None
            else
                nextToken p

                let value = parseExpression p ExprPrecedence.LOWEST

                while not (curTokenIs p TokenType.SEMICOLON) && not (curTokenIs p TokenType.EOF) do
                    nextToken p

                if curTokenIs p TokenType.EOF then
                    p.errors.Add(sprintf "Let statement identified as \"%s\" needs an ending semicolon" identStatement.value)
                    None
                else
                    match value with
                    | Some v ->
                        let letStatement = new Ast.LetStatement(letToken, identStatement, v)
                        Some (letStatement :> Ast.Statement)
                    | None -> None

    let parseReturnStatement p =
        let returnToken = p.curToken

        nextToken p

        let returnValue = parseExpression p ExprPrecedence.LOWEST

        match returnValue with 
        | Some rv ->
            
            while not (curTokenIs p TokenType.SEMICOLON) do
                nextToken p

            let rs = new Ast.ReturnStatement(returnToken, rv)
            Some (rs :> Ast.Statement)
        | None ->
            None

    let parseStringLiteral p =
        new Ast.StringLiteral (p.curToken, p.curToken.Literal)
        |> toSomeExpr

    let parseIdentifier  p =
        new Ast.Identifier (p.curToken, p.curToken.Literal)
        |> toSomeExpr

    let parseIntegerLiteral p =
        match System.Int64.TryParse p.curToken.Literal with
        | true, l ->
            new Ast.IntegerLiteral(p.curToken, l)
            |> toSomeExpr
        | _ -> 
            let errorMsg = sprintf "could not parse %s as integer" p.curToken.Literal
            p.errors.Add(errorMsg)
            None

    let parseBoolean p =
        new Ast.Boolean (p.curToken, curTokenIs p TokenType.TRUE)
        |> toSomeExpr

    let parseExpressionList p endToken =
        let exprList = new ResizeArray<Ast.Expression>()

        if peekTokenIs p endToken then
            nextToken p
            Some (exprList.ToArray())
        else
            nextToken p

            let expr = parseExpression p ExprPrecedence.LOWEST

            if expr.IsSome then
                exprList.Add(expr.Value)
                ()
            else
                ()

            while peekTokenIs p TokenType.COMMA do
                nextToken p
                nextToken p


                let nextExpr = parseExpression p ExprPrecedence.LOWEST
                if nextExpr.IsSome then
                    exprList.Add(nextExpr.Value)
                    ()
                else
                    ()
            
            if not (expectPeek p endToken) then
                None
            else 
                Some (exprList.ToArray())

    let parseArrayLiteral p =
        let curToken = p.curToken

        match parseExpressionList p TokenType.RBRACKET with
        | Some elements ->
            new Ast.ArrayLiteral(curToken, elements)
            |> toSomeExpr
        | None -> None

    let parseHashLiteral p =
        let curToken = p.curToken

        let mutable pairs = Map.empty<Ast.IntegerLiteral, Ast.Expression>
        
        let mutable keepLooping = true
        while not (peekTokenIs p TokenType.RBRACE) && keepLooping do
            nextToken p
            
            let keySome = parseIntegerLiteral p

            if keySome.IsSome then
                let intLit = keySome.Value :?> Ast.IntegerLiteral
                if not (expectPeek p TokenType.COLON) then
                    keepLooping <- false
                    ()
                else
                    nextToken p

                    let valueSome = parseExpression p ExprPrecedence.LOWEST

                    if valueSome.IsSome then
                        let value = valueSome.Value

                        pairs <- pairs.Add(intLit, value)

                        if not (peekTokenIs p TokenType.RBRACE) && not (expectPeek p TokenType.COMMA) then
                            keepLooping <- false
                        else
                            ()
                    else
                        ()
            else
                ()
            
        if not (expectPeek p TokenType.RBRACE) then
            None
        else
            new Ast.HashLiteral(curToken, pairs)
            |> toSomeExpr
    
    let parsePrefixExpression p =
        let curToken = p.curToken

        nextToken p

        match parseExpression p ExprPrecedence.PREFIX with
        | Some expr ->
            new Ast.PrefixExpression(curToken, curToken.Literal, expr)
            |> toSomeExpr
        | None -> None

    let parseInfixExpression p left =
        let curToken = p.curToken

        let precedence = curPrecedence p

        nextToken p

        match parseExpression p precedence with
        | Some right ->
            new Ast.InfixExpression(curToken, left, curToken.Literal, right)
            |> toSomeExpr
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
        | TokenType.LET -> parseLetStatement p
        | TokenType.RETURN -> parseReturnStatement p
        | _ -> parseExpressionStatement p

    let parseBlockStatement p =
        let curToken = p.curToken

        let statements = new ResizeArray<Ast.Statement>()

        nextToken p

        while not (curTokenIs p TokenType.RBRACE) && not (curTokenIs p TokenType.EOF) do
            match parseStatement p with
            | Some statement -> statements.Add(statement)
            | None -> ()

            nextToken p
        
        let statementArray = statements.ToArray()

        let block = new Ast.BlockStatement(curToken, statementArray)

        block

    let parseIfExpression p =
        let curToken = p.curToken

        if not (expectPeek p TokenType.LPAREN) then None
        else
            nextToken p

            let condition = parseExpression p ExprPrecedence.LOWEST

            let notRParen = not (expectPeek p TokenType.RPAREN)
            let notLBrace = not (expectPeek p TokenType.LBRACE)

            if notRParen || notLBrace || condition.IsNone then None
            else 
                let consequence = parseBlockStatement p

                if peekTokenIs p TokenType.ELSE then
                    nextToken p

                    if not (expectPeek p TokenType.LBRACE) then
                        None
                    else
                        let alternative = parseBlockStatement p

                        new Ast.IfExpression(curToken, condition.Value, consequence, Some alternative)
                        |> toSomeExpr
                else
                    new Ast.IfExpression(curToken, condition.Value, consequence, None)
                    |> toSomeExpr

    let parseFunctionParameters p =
        let identifiers = new ResizeArray<Ast.Identifier>()

        if peekTokenIs p TokenType.RPAREN then
            nextToken p
            identifiers.ToArray()
        else
            nextToken p

            let ident = new Ast.Identifier(p.curToken, p.curToken.Literal)
            identifiers.Add(ident)

            while peekTokenIs p TokenType.COMMA do
                nextToken p
                nextToken p
                let iden = new Ast.Identifier(p.curToken, p.curToken.Literal)
                identifiers.Add(iden)
            
            if not (expectPeek p TokenType.RPAREN) then
                Array.empty<Ast.Identifier>
            else identifiers.ToArray()

    let parseFunctionLiteral p =
        let curToken = p.curToken

        if not (expectPeek p TokenType.LPAREN) then
            None
        else

            let parameters = parseFunctionParameters p

            if not (expectPeek p TokenType.LBRACE) then
                None
            else
                let body = parseBlockStatement p

                new Ast.FunctionLiteral(curToken, parameters, body)
                |> toSomeExpr

    let parseCallExpression p (func: Ast.Expression) =
        let curToken = p.curToken
        
        match parseExpressionList p TokenType.RPAREN with
        |  Some arguments ->
            new Ast.CallExpression(curToken, func, arguments)
            |> toSomeExpr
        | None -> None

    let parseIndexExpression p (left: Ast.Expression) =
        let curToken = p.curToken

        nextToken p

        let index = parseExpression p ExprPrecedence.LOWEST

        match index with
        | Some i ->

            if not (expectPeek p TokenType.RBRACKET) then
                None
            else
                new Ast.IndexExpression(curToken, left, i)
                |> toSomeExpr
        | None -> None

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
        prefixFns.Add(TokenType.IF, parseIfExpression)
        prefixFns.Add(TokenType.FUNCTION, parseFunctionLiteral)
        prefixFns.Add(TokenType.STRING, parseStringLiteral)
        prefixFns.Add(TokenType.LBRACKET, parseArrayLiteral)
        prefixFns.Add(TokenType.LBRACE, parseHashLiteral)

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
        infixFns.Add(TokenType.LPAREN, parseCallExpression)
        infixFns.Add(TokenType.LBRACKET, parseIndexExpression)

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
            | Some statement -> 
                list.Add(statement)
            | None -> ()

            nextToken parser

        let statementArray = list.ToArray()

        new Ast.Program(statementArray)




