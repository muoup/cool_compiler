module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

let index_of (l : 'a list) (e : 'a) : int =
    let rec inner_rec (l : 'a list) (i : int) : int =
        match l with
        | x :: _    when x = e -> i
        | _ :: xs              -> inner_rec xs (i + 1)
        | []                   -> -1
    in
    
    inner_rec l 0    

let obj_name_mem_gen (class_name : string) =
    "objname_" ^ class_name
    
let constructor_name_gen (class_name : string) =
    "new." ^ class_name

let method_name_gen (class_name : string) (method_name : string) =
    class_name ^ "." ^ method_name

let vtable_name_gen (class_name : string) =
    ".vtable_" ^ class_name