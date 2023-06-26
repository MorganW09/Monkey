module Evaluator

    let rec eval (node: Ast.Node) =
        match node.AType() with 
        | Ast.Program -> 
            let program = node :?> Ast.Program
            evalStatements program.statements
        | Ast.ExpressionStatement ->
            let exprStmt = node :?> Ast.ExpressionStatement
            eval exprStmt.expression
        | Ast.IntegerLiteral ->
            let intLit = node :?> Ast.IntegerLiteral
            let int = new Object.Integer(intLit.value)
            Some (int :> Object.Object)
        | Ast.Boolean ->
            let boolLit = node :?> Ast.Boolean
            let boolObj = new Object.Boolean(boolLit.value)
            Some (boolObj :> Object.Object)
        | _ -> None

    and evalStatements (stmts: Ast.Statement[]) =
        let mutable result : Object.Object option = None

        for stmt in stmts do
            result <- eval stmt

        result