module StringMap = Map.Make(String)
module StringSet = Set.Make(String)
module StringTbl = Hashtbl.Make(struct
    type t = string
    let equal = String.equal
    let hash = Hashtbl.hash
end)

let rec sliding_window_2 (f : 'a -> 'a -> 'a list) (l : 'a list) : 'a list =
    match l with
    | x :: y :: xs -> 
        begin match f x y with
        | x :: [] -> sliding_window_2 (f) (x :: xs)
        | x :: ys -> x :: sliding_window_2 (f) (ys @ xs)
        | [] -> sliding_window_2 (f) xs
        end
    | _ -> l

let index_of (l : 'a list) (e : 'a) : int =
    let rec inner_rec (l : 'a list) (i : int) : int =
        match l with
        | x :: _    when x = e -> i
        | _ :: xs              -> inner_rec xs (i + 1)
        | []                   -> -1
    in
    
    inner_rec l 0    

let find_index (l : 'a list) (c : 'a -> bool) : int =
    let rec inner_rec (l : 'a list) (i : int) : int =
        match l with
        | x :: _    when c x   -> i
        | _ :: xs              -> inner_rec xs (i + 1)
        | []                   -> -1
    in
    
    inner_rec l 0

let obj_name_mem_gen (class_name : string) =
    ".objname_" ^ class_name
    
let constructor_name_gen (class_name : string) =
    "new." ^ class_name

let method_name_gen (class_name : string) (method_name : string) =
    class_name ^ "." ^ method_name

let vtable_name_gen (class_name : string) =
    ".vtable_" ^ class_name