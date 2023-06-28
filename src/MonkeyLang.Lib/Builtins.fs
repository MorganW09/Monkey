module Builtins

    let isStr  (s: Object.Object) =
        match s with 
        | :? Object.Str as str -> true
        | _ -> false
    let toObj obj = obj :> Object.Object

    let lenFn (objs: Object.Object[]) : Object.Object =
        if objs.Length <> 1 then
            let errorStr = sprintf "wrong number of arguments. got=%d, want=1" objs.Length
            (new Object.Error(errorStr)) :> Object.Object
        else
            let arg = objs.[0]

            if isStr arg then
                let strArg = arg :?> Object.Str
                let length = int64 strArg.value.Length
                
                (new Object.Integer(length)) :> Object.Object
            else
                let errorStr =
                    arg.Type().ToString()
                    |> sprintf "argument to \"len\" not supported, got %s" 
                (new Object.Error(errorStr)) :> Object.Object
    
    let builtinsMap =
        Map.empty
            .Add("len", new Object.Builtin(lenFn))