module Object

    type ObjectType =
    | INTEGER
    | BOOLEAN
    | NULL

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