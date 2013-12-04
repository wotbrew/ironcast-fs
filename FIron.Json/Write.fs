module Write

type JsonVal = | String of string
                | Num of float
                | Bool of bool
                | Array of JsonVal list
                | Obj of (string * JsonVal) list
                | Null
let quote a = "\"" + a + "\""
let rec writei indent v = 
        match v with
        | String s -> quote s
        | Num f -> string f
        | Bool b -> b.ToString().ToLower()
        | Array e -> "[" + String.concat (",\n " + indent) (Seq.map (writei indent) e) + "]"
        | Obj o -> "{\n" + indent + String.concat (",\n" + indent)
                            (Seq.map (fun (a, b) -> indent + "  " + quote a + ": " + writei (indent + "  ") b) o)  + "\n" + indent + "}"
        | Null -> "null"
let write v = writei "" v