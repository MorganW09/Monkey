module Builtins

    let isStr  (s: Object.Object) =
        match s with 
        | :? Object.Str as str -> true
        | _ -> false
    
    let toInt length = new Object.Integer(length)
    let toObj obj = obj :> Object.Object    
    let nullObject = new Object.Null()
    let toIntObj length =
        length
        |> toInt
        |> toObj
    let buildLengthError desiredLength length =
        let errorStr = sprintf "wrong number of arguments. got=%d, want=%d" length desiredLength
        new Object.Error(errorStr)
        |> toObj


    let lenFn (objs: Object.Object[]) : Object.Object =
        if objs.Length <> 1 then
            buildLengthError 1 objs.Length
        else
            let arg = objs.[0]

            match arg.Type() with
            | Object.ObjectType.STRING ->
                let strArg = arg :?> Object.Str
                int64 strArg.value.Length
                |> toIntObj
            | Object.ObjectType.ARRAY ->
                let arrType = arg :?> Object.Array

                int64 arrType.elements.Length
                |> toIntObj
            | _ ->
                let errorStr =
                    arg.Type().ToString()
                    |> sprintf "argument to \"len\" not supported, got %s" 
                new Object.Error(errorStr)
                |> toObj

    let arrayFn (func: Object.Array -> Object.Object) (funcName : string) (objs: Object.Object[]) : Object.Object =
        if objs.Length <> 1 then
            buildLengthError 1 objs.Length
        else
            let arg = objs.[0]

            match arg.Type() with
            | Object.ObjectType.ARRAY ->
                let arr = arg :?> Object.Array
                func arr
            | _ ->
                let errorStr =
                    arg.Type().ToString()
                    |> sprintf "argument to \"%s\" must be ARRAY, got %s" funcName
                new Object.Error(errorStr)
                |> toObj

    let firstExtractor (arr: Object.Array) =
        if arr.elements.Length > 0 then
            arr.elements.[0]
        else
            nullObject
            |> toObj
    
    let firstFn = arrayFn firstExtractor "first"

    let lastExtractor (arr: Object.Array) : Object.Object =
        let len = arr.elements.Length
        if len > 1 then
            arr.elements.[len - 1]
        else if len = 1 then
            arr.elements.[0]
        else
            nullObject
            |> toObj

    let lastFn = arrayFn lastExtractor "last"

    let restExtractor (arr: Object.Array) : Object.Object =
        let len = arr.elements.Length

        if len > 1 then
            let newList = new ResizeArray<Object.Object>()
            for i = 1 to (len - 1) do
                newList.Add(arr.elements.[i])
            
            new Object.Array(newList.ToArray())
            |> toObj

        else if len = 1 then
            let elements = Array.empty<Object.Object>
            new Object.Array(elements)
            |> toObj
        else
            nullObject
            |> toObj

    let restFn = arrayFn restExtractor "rest"

    let pushFn (objs: Object.Object[]) : Object.Object =
        if objs.Length <> 2 then
            buildLengthError 2 objs.Length
        else
            let arg = objs.[0]

            match arg.Type() with
            | Object.ObjectType.ARRAY ->
                let arr = arg :?> Object.Array
                let newElement = objs.[1]

                let newArr = Array.append arr.elements [| newElement |]
                new Object.Array(newArr)
                |> toObj
            | _ ->
                let errorStr =
                    arg.Type().ToString()
                    |> sprintf "argument to \"push\" must be ARRAY, got %s"
                new Object.Error(errorStr)
                |> toObj
    let putsFn (objs: Object.Object[]) : Object.Object =
        for obj in objs do
            let str = obj.Inspect()
            printfn "%s" str
        nullObject
        |> toObj

    let builtinsMap =
        Map.empty
            .Add("len", new Object.Builtin(lenFn))
            .Add("first", new Object.Builtin(firstFn))
            .Add("last", new Object.Builtin(lastFn))
            .Add("rest", new Object.Builtin(restFn))
            .Add("push", new Object.Builtin(pushFn))
            .Add("puts", new Object.Builtin(putsFn))