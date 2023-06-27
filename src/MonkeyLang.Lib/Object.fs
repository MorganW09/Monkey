module Object

    type ObjectType =
    | INTEGER
    | BOOLEAN
    | NULL
    | RETURN
    | ERROR

    type Object =
        abstract member Type : unit -> ObjectType
        abstract member Inspect : unit -> string

    type Integer(value: int64) =
        member this.value = value
        interface Object with
            member this.Inspect () =
                sprintf "%d" this.value
            member this.Type () =
                ObjectType.INTEGER
    
    type Boolean(value: bool) =
        member this.value = value
        interface Object with
            member this.Inspect () =
                sprintf "%b" this.value
            member this.Type() =
                ObjectType.BOOLEAN
    
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

    type Environment() =
        let mutable store = Map.empty<string, Object>
        member this.Get (name: string) =
            match store.ContainsKey(name) with
            | true ->
                Some store.[name]
            | false -> None
        member this.Set (name: string) (value: Object) =
            store <- store.Add(name, value)
            ()
        member this.Count () = store.Count

