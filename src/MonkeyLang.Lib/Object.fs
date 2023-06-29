module Object

    type ObjectType =
    | INTEGER
    | BOOLEAN
    | NULL
    | RETURN
    | ERROR
    | FUNCTION
    | STRING
    | BUILTIN
    | ARRAY
    | HASH

    type Object =
        abstract member Type : unit -> ObjectType
        abstract member Inspect : unit -> string

    type BuiltinFunction = Object[] -> Object

    type Integer(value: int64) =
        member this.value = value
        interface Object with
            member this.Inspect () =
                sprintf "%d" this.value
            member this.Type () =
                ObjectType.INTEGER
        override x.Equals(y) =
            match y with
                | :? Integer as y -> (x = y)
                | _ -> false
        override this.GetHashCode() = 
            this.value.GetHashCode()
        interface System.IComparable with
            member x.CompareTo y =
                match y with
                    | :? Integer as y -> x.value.CompareTo(y.value)
                    | _ -> invalidArg "y" "cannot compare values of different types" 
    
    type Boolean(value: bool) =
        member this.value = value
        interface Object with
            member this.Inspect () =
                sprintf "%b" this.value
            member this.Type() =
                ObjectType.BOOLEAN
    
    type Str(value: string) =
        member this.value = value
        interface Object with
            member this.Inspect() = this.value
            member this.Type() = ObjectType.STRING
    
    type Null() =
        interface Object with
            member this.Inspect () = "null"
            member this.Type() =
                ObjectType.NULL
                
    type Return(value: Object) =
        member this.value = value
        interface Object with
            member this.Inspect () =
                sprintf "%s" (this.value.Inspect())
            member this.Type() =
                ObjectType.RETURN
    
    type Error(message: string) =
        member this.message = message
        interface Object with
            member this.Inspect () =
                sprintf "ERROR: %s" (this.message)
            member this.Type() =
                ObjectType.ERROR

    type Environment(outer: Environment option) =
        let mutable store = Map.empty<string, Object>
        member this.outer = outer
        member this.Get (name: string) =
            match store.ContainsKey(name) with
            | true ->
                Some store.[name]
            | false -> 
                if this.outer.IsSome then
                    let out = outer.Value
                    out.Get name
                else
                    None
        member this.Set (name: string) (value: Object) =
            store <- store.Add(name, value)
            ()
        member this.Count () = store.Count


    type Function(parameters: Ast.Identifier[], body: Ast.BlockStatement, env: Environment) =
        member this.parameters = parameters
        member this.body = body
        member this.env = env
        interface Object with
            member this.Inspect () =
                
                let paraStr = 
                    this.parameters
                        |> Array.map (fun s -> (s :> Ast.Expression).Str())
                        |> Array.reduce (fun a b -> sprintf "%s, %s" a b)
                
                let bodyStr = (this.body :> Ast.Statement).Str()
                sprintf "fn (%s) %s" paraStr bodyStr
            member this.Type() =
                ObjectType.FUNCTION
        
    type Builtin(fn: BuiltinFunction) =
        member this.fn = fn
        interface Object with
            member this.Inspect () = "builtin function"
            member this.Type() = ObjectType.BUILTIN


    type Array(elements: Object[]) =
        member this.elements = elements
        interface Object with
            member this.Type() = ObjectType.ARRAY
            member this.Inspect() =
                let elementStr =
                    this.elements
                    |> Array.map (fun e -> e.Inspect())
                    |> Array.reduce (fun a b -> sprintf "%s, %s" a b)
                
                sprintf "[%s]" elementStr

    type Hash(pairs: Map<Integer, Object>) =
        member this.pairs = pairs
        member this.Get (key: Integer) =
            if this.pairs.ContainsKey key then
                Some this.pairs.[key]
            else
                None
        interface Object with
            member this.Type() =
                ObjectType.HASH
            member this.Inspect () = 
                pairs
                |> Seq.map (fun pair -> (sprintf "%d:%s" pair.Key.value (pair.Value.Inspect())))
                |> Seq.reduce (fun a b -> sprintf "%s, %s" a b)
                |> sprintf "{%s}"