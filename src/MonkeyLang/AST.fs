module AST

open Tokens



type Node =
    abstract member TokenLiteral : unit -> string


type Statement =
    inherit Node 
    abstract member statementNode : unit -> unit

    //Node

type Expression =
    inherit Node
    abstract member expressionNode : unit -> unit

//type Identifier = 
//    {
//        Token : TokenType
//        Value : string
//    }
type Identifier(Token : Token, Value : string) =
    interface Expression with
        member this.TokenLiteral(): string = Token.Literal
        member this.expressionNode(): unit = ()

type LetStatement(Token : Token, Name : Identifier, Value : Expression) =
    interface Statement with
        member this.TokenLiteral () = Token.Literal
        member this.statementNode(): unit = ()
        


//type 
type Program =
    {
        Statements : Statement list 
    }

let TokenLiteral (program: Program) =
    if program.Statements.Length > 0 then
        program.Statements.Head.TokenLiteral()
    else
        ""