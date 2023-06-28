module Ast
    type AstType =
    | Program
    | Identifier
    | IntegerLiteral
    | PrefixExpression
    | InfixExpression
    | LetStatement
    | ReturnStatement
    | ExpressionStatement
    | Boolean
    | BlockStatement
    | IfExpression
    | FunctionLiteral
    | CallExpression
    | StringLiteral
    | ArrayLiteral
    | IndexExpression

    type Node =
        abstract member TokenLiteral : unit -> string
        abstract member Str : unit -> string
        abstract member AType : unit -> AstType
    
    type Statement =
        inherit Node
        abstract member statementNode: unit -> unit

    type Expression =
        inherit Node
        abstract member expressionNode: unit -> unit
    
    type Program(statements : Statement []) =
        member this.statements = statements
        interface Node with 
            member this.AType () = AstType.Program
            member this.TokenLiteral () =
                match this.statements.Length with 
                | 0 -> ""
                | _ -> 
                    let firstStatement = this.statements.[0]
                    (firstStatement :> Node).TokenLiteral()
            member this.Str () =
                this.statements
                    |> Array.map (fun s -> s.Str())
                    |> Array.reduce (fun a b -> a + b)
    
    type Identifier(token: Tokens.Token, value: string) =
        member this.token = token
        member this.value = value
        interface Expression with
            member this.AType () = AstType.Identifier
            member this.TokenLiteral () = token.Literal
            member this.expressionNode () = ()
            member this.Str () = value

    type IntegerLiteral(token: Tokens.Token, value: int64) =
        member this.token = token
        member this.value = value
        interface Expression with
            member this.AType () = AstType.IntegerLiteral
            member this.TokenLiteral () = token.Literal
            member this.expressionNode () = ()
            member this.Str () = sprintf "%d" value

    type PrefixExpression(token: Tokens.Token, operator: string, right: Expression) =
        member this.token = token
        member this.operator = operator
        member this.right = right
        interface Expression with
            member this.AType () = AstType.PrefixExpression
            member this.TokenLiteral () = token.Literal
            member this.expressionNode () = ()
            member this.Str () = 
                let rightStr = this.right.Str()

                sprintf "(%s%s)" this.operator rightStr

    type InfixExpression(token: Tokens.Token, left: Expression, operator: string, right: Expression) =
        member this.token = token
        member this.left = left
        member this.operator = operator
        member this.right = right
        interface Expression with
            member this.AType () = AstType.InfixExpression
            member this.TokenLiteral () = token.Literal
            member this.expressionNode () = ()
            member this.Str () = 
                let leftStr = this.left.Str()
                let rightStr = this.right.Str()

                sprintf "(%s %s %s)"  leftStr this.operator rightStr

    
    type LetStatement(token: Tokens.Token, name: Identifier, value: Expression) =
        member this.token = token
        member this.name = name
        member this.value = value
        interface Statement with
            member this.AType () = AstType.LetStatement
            member this.TokenLiteral () = token.Literal
            member this.statementNode () = ()
            member this.Str () =
                sprintf "%s %s = %s;" (this.token.Literal) ((this.name :> Expression).Str()) (this.value.Str())

    type ReturnStatement(token: Tokens.Token, returnValue: Expression) =
        member this.token = token
        member this.returnValue = returnValue
        interface Statement with
            member this.AType () = AstType.ReturnStatement
            member this.TokenLiteral () = token.Literal
            member this.statementNode () = ()
            member this.Str () =
                sprintf "%s %s;" (this.token.Literal) (this.returnValue.Str())

    type ExpressionStatement(token: Tokens.Token, expression: Expression) =
        member this.token = token
        member this.expression = expression
        interface Statement with
            member this.AType () = AstType.ExpressionStatement
            member this.TokenLiteral () = token.Literal
            member this.statementNode () = ()
            member this.Str () = this.expression.Str()

    type Boolean(token: Tokens.Token, value: bool) =
        member this.token = token
        member this.value = value
        interface Expression with
            member this.AType () = AstType.Boolean
            member this.TokenLiteral () = this.token.Literal
            member this.expressionNode () = ()
            member this.Str () = this.token.Literal

    type BlockStatement(token: Tokens.Token, statements: Statement[]) =
        member this.token = token
        member this.statements = statements
        interface Statement with
            member this.AType () = AstType.BlockStatement
            member this.TokenLiteral () = this.token.Literal
            member this.statementNode () = ()                
            member this.Str () =
                this.statements
                    |> Array.map (fun s -> s.Str())
                    |> Array.reduce (fun a b -> a + b)

    type IfExpression(token: Tokens.Token, condition: Expression, consequence: BlockStatement, alternative: BlockStatement option) =
        member this.token = token
        member this.condition = condition
        member this.consequence = consequence
        member this.alternative = alternative
        interface Expression with
            member this.AType () = AstType.IfExpression
            member this.TokenLiteral () = this.token.Literal
            member this.expressionNode () = ()
            member this.Str () = 
                let ifStr = this.condition.Str()
                let consequenceStr = (this.consequence :> Statement).Str()

                let firstStr = sprintf "if%s %s" ifStr consequenceStr

                match this.alternative with
                | Some alt ->
                    let altStr = (alt :> Statement).Str()
                    sprintf "%selse %s" firstStr altStr
                | None -> firstStr
    
    type FunctionLiteral(token: Tokens.Token, parameters: Identifier[], body: BlockStatement) =
        member this.token = token
        member this.parameters = parameters
        member this.body = body
        interface Expression with
            member this.AType () = AstType.FunctionLiteral
            member this.TokenLiteral () = this.token.Literal
            member this.expressionNode () = ()
            member this.Str () = 
                let paraStr = 
                    this.parameters
                        |> Array.map (fun s -> (s :> Expression).Str())
                        |> Array.reduce (fun a b -> sprintf "%s, %s" a b)
                
                let bodyStr = (this.body :> Statement).Str()
                sprintf "%s (%s) %s" this.token.Literal paraStr bodyStr
    
    type CallExpression(token: Tokens.Token, func: Expression, arguments: Expression[]) =
        member this.token = token
        member this.func = func
        member this.arguments = arguments
        interface Expression with
            member this.AType () = AstType.CallExpression
            member this.TokenLiteral () = this.token.Literal
            member this.expressionNode () = ()
            member this.Str () = 
                let argStr =
                    this.arguments
                        |> Array.map (fun s -> s.Str())
                        |> Array.reduce (fun a b -> sprintf "%s, %s" a b)
                
                let funcStr = this.func.Str()

                sprintf "%s(%s)" funcStr argStr
    
    type StringLiteral(token: Tokens.Token, value: string) =
        member this.token = token
        member this.value = value
        interface Expression with
            member this.AType () = AstType.StringLiteral
            member this.TokenLiteral () = this.token.Literal
            member this.expressionNode () = ()
            member this.Str () = this.token.Literal

    type ArrayLiteral(token: Tokens.Token, elements: Expression[]) =
        member this.token = token
        member this.elements = elements
        interface Expression with
            member this.AType () = AstType.ArrayLiteral
            member this.TokenLiteral () = this.token.Literal
            member this.expressionNode () = ()
            member this.Str () = 
                let elementStr = 
                    this.elements
                    |> Array.map (fun e -> e.Str())
                    |> Array.reduce (fun a b -> a + ", " + b)
                
                sprintf "[%s]" elementStr
    
    type IndexExpression(token: Tokens.Token, left: Expression, index: Expression) =
        member this.token = token
        member this.left = left
        member this.index = index
        interface Expression with
            member this.AType () = AstType.IndexExpression
            member this.TokenLiteral () = this.token.Literal
            member this.expressionNode () = ()
            member this.Str () = 
                let leftStr = this.left.Str()
                let indexStr = this.index.Str()
                
                sprintf "(%s[%s])" leftStr indexStr


