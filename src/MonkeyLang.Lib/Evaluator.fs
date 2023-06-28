module Evaluator

    let trueBooleanObject = new Object.Boolean(true)
    let falseBooleanObject = new Object.Boolean(false)
    let nullObject = new Object.Null()

    let toObj obj = obj :> Object.Object

    let toSomeObj obj = Some (obj :> Object.Object)

    
    let canDowncastToFunction (s: Object.Object) =
        match s with 
        | :? Object.Function as func -> true
        | _ -> false
    let canDowncastToBuiltin (s: Object.Object) =
        match s with 
        | :? Object.Builtin as func -> true
        | _ -> false
    let canDowncastToReturn (s: Object.Object) =
        match s with 
        | :? Object.Return as retr -> true
        | _ -> false

    let isTruthy (obj : Object.Object)  =
        match obj.Type() with
        | Object.NULL -> false
        | Object.BOOLEAN ->
            let bool = obj :?> Object.Boolean
            bool.value
        | _ -> true

    let getTypeStr obj =
        (obj :> Object.Object).Type().ToString()

    let newError message =
        new Object.Error(message)

    let isError (obj: Object.Object) =
        obj.Type() = Object.ObjectType.ERROR

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
        | _ -> 
            getTypeStr right
            |> sprintf "unknown operator: -%s"
            |> newError
            |> toObj


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
        | _ -> 
            let leftStr = getTypeStr left
            let rightStr = getTypeStr right

            sprintf "unknown operator: %s %s %s" leftStr operator rightStr
            |> newError
            |> toObj

    let evalBooleanInfixExpression operator (left: Object.Boolean) (right: Object.Boolean) =
        match operator with
        | "==" ->
            new Object.Boolean(left.value = right.value)
            |> toObj
        | "!=" -> 
            new Object.Boolean(left.value <> right.value)
            |> toObj
        | _ ->
            let leftStr = getTypeStr left
            let rightStr = getTypeStr right

            sprintf "unknown operator: %s %s %s" leftStr operator rightStr
            |> newError
            |> toObj

    let evalStringInfixExpression operator (left: Object.Str) (right: Object.Str) =
        match operator with
        | "+" ->
            new Object.Str(left.value + right.value)
            |> toObj
        | _ ->
            let leftStr = getTypeStr left
            let rightStr = getTypeStr right

            sprintf "unknown operator: %s %s %s" leftStr operator rightStr
            |> newError
            |> toObj


    let evalPrefixExpression operator right =
        match operator with
        | "!" ->
            let booleanObject = evalBangOperatorExpression right
            booleanObject :> Object.Object
        | "-" ->
            evalMinusPrefixOperatorExpression right
        | _ -> 
            getTypeStr right
            |> sprintf "unknown operator: %s%s" operator
            |> newError
            |> toObj

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
        | Object.ObjectType.STRING, Object.ObjectType.STRING ->
            let leftStr = left :?> Object.Str
            let rightStr = right :?> Object.Str
            evalStringInfixExpression operator leftStr rightStr
        | _, _ ->             
            let leftStr = getTypeStr left
            let rightStr = getTypeStr right
            let mutable errorString = ""

            if left.Type() <> right.Type() then
                errorString <- (sprintf "type mismatch: %s %s %s" leftStr operator rightStr)
                ()
            else 
                errorString <- sprintf "unknown operator: %s %s %s" leftStr operator rightStr
                ()
            errorString
            |> newError
            |> toObj
    
    let evalIdentifier (identifier: string) (env: Object.Environment) =
        let value = env.Get identifier

        match value with
        | Some v ->
            v
        | None ->

            if Builtins.builtinsMap.ContainsKey identifier then
                Builtins.builtinsMap.[identifier]
                |> toObj
            else
                sprintf "identifier not found: %s" identifier
                |> newError
                |> toObj

    let extendFunctionEnv (fn: Object.Function) (args: Object.Object[]) =
        let env = new Object.Environment(Some fn.env)

        for i = 0 to (fn.parameters.Length - 1) do
            let param = fn.parameters.[i].value
            env.Set param args.[i]

        env

    let unwrapReturnValue (obj: Object.Object option) =
        if obj.IsSome && canDowncastToReturn obj.Value then
            let returnStmt = obj.Value :?> Object.Return
            Some returnStmt.value
        else
            obj

    let rec eval (node: Ast.Node) (env: Object.Environment) =
        match node.AType() with 
        | Ast.Program -> 
            let program = node :?> Ast.Program
            evalProgram program.statements env
        | Ast.ExpressionStatement ->
            let exprStmt = node :?> Ast.ExpressionStatement
            eval exprStmt.expression env
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
            let right = eval preExpr.right env

            if right.IsSome then
                let rValue = right.Value

                match isError rValue with
                | true ->
                    Some rValue
                | false ->
                    let result = evalPrefixExpression preExpr.operator rValue
                    Some result
            else None
        | Ast.InfixExpression ->
            let inExpr = node :?> Ast.InfixExpression

            let left = eval inExpr.left env
            let right = eval inExpr.right env

            match left, right with
            | Some l, Some r ->

                match isError(l), isError(r) with
                | true, _ ->
                    Some l
                | false, true ->
                    Some r
                | false, false ->
                    let result = evalInfixExpression inExpr.operator l r
                    Some result
            | _, _ -> None
        | Ast.BlockStatement ->
            let block = node :?> Ast.BlockStatement
            evalBlockStatement block env
        | Ast.IfExpression ->
            let ifExpr = node :?> Ast.IfExpression
            evalIfExpression ifExpr env
        | Ast.ReturnStatement ->
            let rtnStmt = node :?> Ast.ReturnStatement
            let result = eval rtnStmt.returnValue env

            if result.IsSome then
                let value = result.Value
                match isError value with
                | true ->
                    toSomeObj value
                | false -> 
                    let returnValue = new Object.Return(value)
                    toSomeObj returnValue
            else 
                None
        | Ast.LetStatement ->
            let letStmt = node :?> Ast.LetStatement
            let result = eval letStmt.value env

            if result.IsSome then
                let value = result.Value

                match isError value with
                | true ->
                    toSomeObj value
                | false ->
                    env.Set letStmt.name.value value
                    None
            else
                None
        | Ast.Identifier ->
            let iden = node :?> Ast.Identifier

            evalIdentifier iden.value env
            |> toSomeObj
        | Ast.FunctionLiteral ->
            let func = node :?> Ast.FunctionLiteral

            let param = func.parameters
            let body = func.body

            new Object.Function(param, body, env)
            |> toSomeObj
        | Ast.CallExpression ->
            let callExpr = node :?> Ast.CallExpression

            let func = eval callExpr.func env

            if func.IsSome then
                let funcValue = func.Value

                match isError funcValue with
                | true ->
                    toSomeObj funcValue
                | false ->
                    let args: Object.Object array = evalExpressions callExpr.arguments env

                    if args.Length = 1 && isError args.[0] then
                        toSomeObj args.[0]
                    else
                        applyFunction funcValue args
            else
                None
        | Ast.StringLiteral ->
            let str = node :?> Ast.StringLiteral
            new Object.Str(str.value)
            |> toSomeObj

    and evalProgram (stmts: Ast.Statement[]) (env: Object.Environment) =
        let mutable result : Object.Object option = None
        let mutable earlyReturn : Object.Object option = None

        for stmt in stmts do
            result <- eval stmt env

            //return early if return or error
            if result.IsSome  && earlyReturn.IsNone then
                let rs = result.Value
                match rs.Type() with
                | Object.ObjectType.RETURN ->
                    earlyReturn <- Some rs
                    ()
                | Object.ObjectType.ERROR ->
                    earlyReturn <- Some rs
                    ()
                | _ -> ()
                    
                ()
            else 
                ()
        
        match earlyReturn with
        | Some er ->
            match er.Type() with
            | Object.ObjectType.RETURN ->
                let rtrStmt = (er :?> Object.Return)
                Some rtrStmt.value
            | Object.ObjectType.ERROR ->
                Some er
            | _ -> result
        | None -> result
    
    and evalBlockStatement (block: Ast.BlockStatement) (env: Object.Environment) =
        let mutable result : Object.Object option = None
        let mutable earlyReturn : Object.Object option = None

        for stmt in block.statements do
            result <- (eval stmt env)

            match result with
            | Some r ->
                if earlyReturn.IsNone && (r.Type() = Object.RETURN || r.Type() = Object.ERROR)  then
                    earlyReturn <- Some r
                    ()
                else
                    ()
            | None ->
                ()
        
        if earlyReturn.IsSome then
            earlyReturn
        else
            result

    and evalIfExpression (ifExpr: Ast.IfExpression) (env: Object.Environment) =
        let condition = eval ifExpr.condition env

        match condition with
        | Some c ->
            if isError c then
                Some c
            else if isTruthy c then
                eval ifExpr.consequence env
            else if ifExpr.alternative.IsSome then
                eval ifExpr.alternative.Value env
            else Some (nullObject |> toObj)
        | None -> Some (nullObject |> toObj)

    and evalExpressions (exps: Ast.Expression[]) (env: Object.Environment) =
        let results = new ResizeArray<Object.Object>()
        let errorResult = new ResizeArray<Object.Object>()

        for exp in exps do
            let evaluated = eval exp env

            match evaluated with
            | Some ev ->
                if isError ev && errorResult.Count = 0 then
                    errorResult.Add(ev)
                    ()
                else
                    results.Add(ev)
                    ()
            | None ->
                ()
        
        if errorResult.Count > 0 then
            errorResult.ToArray()
        else
            results.ToArray()
    
    and applyFunction (fn: Object.Object) (args: Object.Object[]) =
        if canDowncastToFunction fn then
            let func = fn :?> Object.Function

            let extendedEnv = extendFunctionEnv func args
            let evaluated = eval func.body extendedEnv

            unwrapReturnValue evaluated
        else if canDowncastToBuiltin fn then
            let builtin = fn :?> Object.Builtin

            builtin.fn args
            |> toSomeObj
        else
            let typeStr = fn.Type().ToString()
            sprintf "not a function: %s" typeStr
            |> newError 
            |> toSomeObj

    let evaluate (node: Ast.Node) =
        let env = new Object.Environment(None)
        eval node env