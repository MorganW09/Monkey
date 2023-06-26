module Evaluator

    

    let trueBooleanObject = new Object.Boolean(true)
    let falseBooleanObject = new Object.Boolean(false)
    let nullObject = new Object.Null()

    let toObj obj = obj :> Object.Object

    let isTruthy (obj : Object.Object)  =
        match obj.Type() with
        | Object.NULL -> false
        | Object.BOOLEAN ->
            let bool = obj :?> Object.Boolean
            bool.value
        | _ -> true

    let nativeBoolToBooleanObject input =
        if input then trueBooleanObject
        else falseBooleanObject

    let evalBangOperatorExpression (right: Object.Object) =
        match right.Type() with
        | Object.ObjectType.BOOLEAN ->
            let bool = right :?> Object.Boolean

            nativeBoolToBooleanObject (not bool.value)
        | Object.ObjectType.NULL -> trueBooleanObject
        | _ -> falseBooleanObject

    let evalMinusPrefixOperatorExpression (right: Object.Object) =
        match right.Type() with
        | Object.ObjectType.INTEGER ->
            let int = right :?> Object.Integer

            let reversedInt = int.value * -1L

            let reversedObj = new Object.Integer(reversedInt)

            (reversedObj :> Object.Object)
        | _ -> (nullObject :> Object.Object)

    let evalIntegerInfixExpression operator (left: Object.Integer) (right: Object.Integer) =
        match operator with
        | "+" -> 
            new Object.Integer(left.value + right.value)
            |> toObj
        | "-" -> 
            new Object.Integer(left.value - right.value)
            |> toObj
        | "*" -> 
            new Object.Integer(left.value * right.value)
            |> toObj
        | "/" -> 
            new Object.Integer(left.value / right.value)
            |> toObj
        | "<" ->
            nativeBoolToBooleanObject(left.value < right.value)
            |> toObj
        | ">" ->
            nativeBoolToBooleanObject(left.value > right.value)
            |> toObj
        | "==" ->
            nativeBoolToBooleanObject(left.value = right.value)
            |> toObj
        | "!=" -> 
            nativeBoolToBooleanObject(left.value <> right.value)
            |> toObj
        | _ -> toObj nullObject

    let evalBooleanInfixExpression operator (left: Object.Boolean) (right: Object.Boolean) =
        match operator with
        | "==" ->
            new Object.Boolean(left.value = right.value)
            |> toObj
        | "!=" -> 
            new Object.Boolean(left.value <> right.value)
            |> toObj
        | _ -> toObj nullObject


    let evalPrefixExpression operator right =
        match operator with
        | "!" ->
            let booleanObject = evalBangOperatorExpression right
            booleanObject :> Object.Object
        | "-" ->
            evalMinusPrefixOperatorExpression right
        | _ -> nullObject :> Object.Object

    let evalInfixExpression operator (left: Object.Object) (right: Object.Object) =
        match left.Type(), right.Type() with
        | Object.ObjectType.INTEGER, Object.ObjectType.INTEGER ->
            let leftInt = left :?> Object.Integer
            let rightInt = right :?> Object.Integer
            evalIntegerInfixExpression operator leftInt rightInt
        | Object.ObjectType.BOOLEAN, Object.ObjectType.BOOLEAN ->
            let leftBool = left :?> Object.Boolean
            let rightBool = right :?> Object.Boolean
            evalBooleanInfixExpression operator leftBool rightBool
        | _ -> nullObject :> Object.Object

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
            let boolObj = 
                if boolLit.value then trueBooleanObject
                else falseBooleanObject
            Some (boolObj :> Object.Object)
        | Ast.PrefixExpression ->
            let preExpr = node :?> Ast.PrefixExpression
            let right = eval preExpr.right

            match right with
            | Some r ->
                let result = evalPrefixExpression preExpr.operator r
                Some result
            | None -> None
        | Ast.InfixExpression ->
            let inExpr = node :?> Ast.InfixExpression

            let left = eval inExpr.left
            let right = eval inExpr.right

            match left, right with
            | Some l, Some r ->
                let result = evalInfixExpression inExpr.operator l r
                Some result
            | _, _ -> None
        | Ast.BlockStatement ->
            let block = node :?> Ast.BlockStatement
            evalStatements block.statements
        | Ast.IfExpression ->
            let ifExpr = node :?> Ast.IfExpression
            evalIfExpression ifExpr
        | _ -> None

    and evalStatements (stmts: Ast.Statement[]) =
        let mutable result : Object.Object option = None

        for stmt in stmts do
            result <- eval stmt

        result
    and evalIfExpression (ifExpr: Ast.IfExpression) =
        let condition = eval ifExpr.condition

        match condition with
        | Some c ->
            if isTruthy c then
                eval ifExpr.consequence
            else if ifExpr.alternative.IsSome then
                eval ifExpr.alternative.Value
            else Some (nullObject |> toObj)
        | None -> Some (nullObject |> toObj)