open A_util

type ssa_id =
    | Local of int
    | Attribute of int
    | Parameter of int
    
    | IntLiteral of int
    | StringLiteral of string
    | BoolLiteral of bool

    | Unit
    
    | Self

type symbol_table = ssa_id StringTbl.t

type ssa_stmt = { id : ssa_id; _val : ssa_val }

and ssa_val =
    | SSA_add of ssa_id * ssa_id
    | SSA_sub of ssa_id * ssa_id
    | SSA_mul of ssa_id * ssa_id
    | SSA_div of int * ssa_id * ssa_id

    | SSA_lt  of ssa_id * ssa_id
    | SSA_lte of ssa_id * ssa_id
    | SSA_eq  of ssa_id * ssa_id

    | SSA_ident of ssa_id

    | SSA_neg of ssa_id
    | SSA_not of ssa_id

    | SSA_new of string
    | SSA_default of string

    | SSA_isvoid of ssa_id

    | SSA_call of string * ssa_id list
    | SSA_dispatch of { line_number : int; obj : ssa_id; method_id : int; args : ssa_id list }

    | SSA_label of string
    | SSA_jmp of string
    | SSA_bt of ssa_id * string

    | SSA_object of string * int
    | SSA_attribute of { object_id : ssa_id; attribute_id : int; value : ssa_id }
    
    | SSA_internal of string

    | SSA_return of ssa_id
    | SSA_comment of string

    | SSA_default_mem of string
    | SSA_valued_mem of ssa_id * string

    | SSA_store of ssa_id * ssa_id
    | SSA_load of ssa_id

    (* Special Internal Nodes *)
    | SSA_str_eq  of ssa_id * ssa_id

type method_ssa = {
    class_name : string;
    method_name : string;
    arg_count : int;

    stmts : ssa_stmt list;
    ids : ssa_id list
}

let f_sid (id : ssa_id) : string =
    match id with
    | Local i -> Printf.sprintf "%%%d" i
    | Attribute i -> Printf.sprintf "A%d" i
    | Parameter i -> Printf.sprintf "P%d" i

    | IntLiteral i -> Printf.sprintf "%d" i
    | StringLiteral s -> Printf.sprintf "\"%s\"" s
    | BoolLiteral b -> Printf.sprintf "%b" b

    | Unit -> "!"

    | Self -> "self"

let val_as_str (_val : ssa_val) : string =
    match _val with
    | SSA_add (a, b) -> (Printf.sprintf "%s + %s" (f_sid a) (f_sid b))
    | SSA_sub (a, b) -> (Printf.sprintf "%s - %s" (f_sid a) (f_sid b))
    | SSA_mul (a, b) -> (Printf.sprintf "%s * %s" (f_sid a) (f_sid b))
    | SSA_div (_, a, b) -> (Printf.sprintf "%s / %s" (f_sid a) (f_sid b))

    | SSA_lt (a, b) -> (Printf.sprintf "%s < %s" (f_sid a) (f_sid b))
    | SSA_lte (a, b) -> (Printf.sprintf "%s <= %s" (f_sid a) (f_sid b))
    | SSA_eq (a, b) -> (Printf.sprintf "%s == %s" (f_sid a) (f_sid b))

    | SSA_ident a -> (Printf.sprintf "%s" (f_sid a))
    | SSA_neg a -> (Printf.sprintf "-%s" (f_sid a))
    | SSA_not a -> (Printf.sprintf "not %s" (f_sid a))

    | SSA_new a -> (Printf.sprintf "new %s" a)
    | SSA_default a -> (Printf.sprintf "default %s" a)
    | SSA_isvoid a -> (Printf.sprintf "isvoid %s" (f_sid a))

    | SSA_call (method_name, args) -> 
        (Printf.sprintf "%s(%s)" (method_name) (String.concat ", " (List.map f_sid args)))
    | SSA_dispatch { line_number; obj; method_id; args } -> 
        (Printf.sprintf "%s.%d(%s)" (f_sid obj) method_id (String.concat ", " (List.map f_sid args)))
        
    | SSA_label a -> (Printf.sprintf "label %s" a)
    | SSA_jmp a -> (Printf.sprintf "jmp %s" a)
    | SSA_bt (a, b) -> (Printf.sprintf "bt %s %s" (f_sid a) b)

    | SSA_object (a, b) -> (Printf.sprintf "%s.%d" a b)
    | SSA_attribute { object_id; attribute_id; value } -> 
        (Printf.sprintf "%s.%d = %s" (f_sid object_id) attribute_id (f_sid value))
    | SSA_internal a -> (Printf.sprintf "internal %s" a)

    | SSA_return a -> (Printf.sprintf "return %s" (f_sid a))
    | SSA_comment a -> (Printf.sprintf "comment %s" a)

    | SSA_default_mem (type_name) -> (Printf.sprintf "alloc default; type = %s" type_name)
    | SSA_valued_mem (a, type_name) -> (Printf.sprintf "alloc %s; type = %s" (f_sid a) type_name)

    | SSA_store (a, b) -> (Printf.sprintf "store %s -> %s" (f_sid a) (f_sid b))
    | SSA_load a -> (Printf.sprintf "load %s" (f_sid a))

    | SSA_str_eq (a, b) -> (Printf.sprintf "%s == %s" (f_sid a) (f_sid b))

let referenced_ids (stmt : ssa_stmt) : ssa_id list =
    stmt.id :: 
    match stmt._val with
    | SSA_add (a, b) -> [a; b]
    | SSA_sub (a, b) -> [a; b]
    | SSA_mul (a, b) -> [a; b]
    | SSA_div (_, a, b) -> [a; b]

    | SSA_lt (a, b) -> [a; b]
    | SSA_lte (a, b) -> [a; b]
    | SSA_eq (a, b) -> [a; b]

    | SSA_ident a -> [a]
    | SSA_neg a -> [a]
    | SSA_not a -> [a]

    | SSA_new _ -> []
    | SSA_default _ -> []
    | SSA_isvoid a -> [a]

    | SSA_call (_, args) -> args
    | SSA_dispatch { obj; args } -> obj :: args

    | SSA_label _ -> []
    | SSA_jmp _ -> []
    | SSA_bt (a, _) -> [a]

    | SSA_object (_, _) -> []
    | SSA_attribute { object_id; value } -> object_id :: [value]
    
    | SSA_internal _ -> []

    | SSA_return a -> [a]
    | SSA_comment _ -> []

    | SSA_default_mem _ -> []
    | SSA_valued_mem (a, _) -> [a]

    | SSA_store (a, b) -> [a; b]
    | SSA_load a -> [a]

    | SSA_str_eq (a, b) -> [a; b]

let print_ssa_stmt (output : string -> unit) (stmt : ssa_stmt) : unit =
    output (Printf.sprintf "%s = %s" (f_sid stmt.id) (val_as_str stmt._val));