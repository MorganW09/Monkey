module Ast
    type Node =
        abstract member TokenLiteral : unit -> string
    
    type Statement =
        inherit Node
        abstract member statementNode: unit -> unit

    type Expression =
        inherit Node
        abstract member expressionNode: unit -> unit

    type LineType =
        | Statement
        | Expression
    
    type Program =
        {
            Statements : Statement []
        }
    
    type Identifier(token: Tokens.Token, value: string) =
        member this.token = token
        member this.value = value
        interface Expression with
            member this.TokenLiteral () = token.Literal
            member this.expressionNode () = ()

    type LetStatement(token: Tokens.Token, name: Identifier, value: Expression) =
        member this.token = token
        member this.name = name
        member this.value = value
        interface Statement with
            member this.TokenLiteral () = token.Literal
            member this.statementNode () = ()

    
    let TokenLiteral (p: Program) =
        if (p.Statements.Length > 0) then 
            let statement = p.Statements.[0]
            statement.TokenLiteral()
        else
            ""

