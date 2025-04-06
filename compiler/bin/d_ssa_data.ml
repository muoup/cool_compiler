open A_util

type ssa_id =
    | Local of int
    | Temporary of int
    | Attribute of int
    | Parameter of int
    | Self

type symbol_table = ssa_id StringTbl.t

type ssa_stmt =
    | SSA_add of ssa_id * ssa_id * ssa_id
    | SSA_sub of ssa_id * ssa_id * ssa_id
    | SSA_mul of ssa_id * ssa_id * ssa_id
    | SSA_div of int * ssa_id * ssa_id * ssa_id

    | SSA_lt  of ssa_id * ssa_id * ssa_id
    | SSA_lte of ssa_id * ssa_id * ssa_id
    | SSA_eq  of ssa_id * ssa_id * ssa_id

    | SSA_int of ssa_id * int
    | SSA_str of ssa_id * string
    | SSA_bool of ssa_id * bool
    | SSA_ident of ssa_id * ssa_id

    | SSA_neg of ssa_id * ssa_id
    | SSA_not of ssa_id * ssa_id

    | SSA_new of ssa_id * string
    | SSA_default of ssa_id * string

    | SSA_isvoid of ssa_id * ssa_id

    | SSA_call of ssa_id * string * ssa_id list
    | SSA_dispatch of { line_number : int; store : ssa_id; obj : ssa_id; method_id : int; args : ssa_id list }

    | SSA_label of string
    | SSA_jmp of string
    | SSA_bt of ssa_id * string

    | SSA_object of ssa_id * string * int
    | SSA_attribute of { object_id : ssa_id; attribute_id : int; value : ssa_id }
    
    | SSA_internal of string

    | SSA_return of ssa_id
    | SSA_comment of string

    | SSA_stack_slot of ssa_id
    | SSA_store of ssa_id * ssa_id
    | SSA_load of ssa_id * ssa_id

    (* Special Internal Nodes *)
    | SSA_str_eq  of ssa_id * ssa_id * ssa_id

type method_ssa = {
    class_name : string;
    method_name : string;
    arg_count : int;

    commands : ssa_stmt list;
    ids : ssa_id list
}

let f_id (id : ssa_id) : string =
    match id with
    | Local i -> Printf.sprintf "L%d" i
    | Temporary i -> Printf.sprintf "T%d" i
    | Attribute i -> Printf.sprintf "A%d" i
    | Parameter i -> Printf.sprintf "P%d" i
    | Self -> "self"

let print_ssa_stmt (output : string -> unit) (stmt : ssa_stmt) : unit =
    match stmt with
    | SSA_add (a, b, c) -> output (Printf.sprintf "%s = %s + %s" (f_id a) (f_id b) (f_id c))
    | SSA_sub (a, b, c) -> output (Printf.sprintf "%s = %s - %s" (f_id a) (f_id b) (f_id c))
    | SSA_mul (a, b, c) -> output (Printf.sprintf "%s = %s * %s" (f_id a) (f_id b) (f_id c))
    | SSA_div (_, a, b, c) -> output (Printf.sprintf "%s = %s / %s" (f_id a) (f_id b) (f_id c))

    | SSA_lt (a, b, c) -> output (Printf.sprintf "%s = %s < %s" (f_id a) (f_id b) (f_id c))
    | SSA_lte (a, b, c) -> output (Printf.sprintf "%s = %s <= %s" (f_id a) (f_id b) (f_id c))
    | SSA_eq (a, b, c) -> output (Printf.sprintf "%s = %s == %s" (f_id a) (f_id b) (f_id c))

    | SSA_int (a, b) -> output (Printf.sprintf "%s = %d" (f_id a) b)
    | SSA_str (a, b) -> output (Printf.sprintf "%s = \"%s\"" (f_id a) b)
    | SSA_bool (a, b) -> output (Printf.sprintf "%s = %b" (f_id a) b)

    | SSA_ident (a, b) -> output (Printf.sprintf "%s = %s" (f_id a) (f_id b))
    | SSA_neg (a, b) -> output (Printf.sprintf "%s = -%s" (f_id a) (f_id b))
    | SSA_not (a, b) -> output (Printf.sprintf "%s = not %s" (f_id a) (f_id b))

    | SSA_new (a, b) -> output (Printf.sprintf "%s = new %s" (f_id a) b)
    | SSA_default (a, b) -> output (Printf.sprintf "%s = default %s" (f_id a) b)
    | SSA_isvoid (a, b) -> output (Printf.sprintf "%s = isvoid %s" (f_id a) (f_id b))

    | SSA_call (id, method_name, args) -> output (Printf.sprintf "%s = %s(%s)" (f_id id) (method_name) (String.concat ", " (List.map f_id args)))
    | SSA_dispatch { line_number; store; obj; method_id; args } -> 
        output (Printf.sprintf "%s = %s.%d(%s)" (f_id store) (f_id obj) method_id (String.concat ", " (List.map f_id args)))
        
    | SSA_label a -> output (Printf.sprintf "label %s" a)
    | SSA_jmp a -> output (Printf.sprintf "jmp %s" a)
    | SSA_bt (a, b) -> output (Printf.sprintf "bt %s %s" (f_id a) b)

    | SSA_object (a, b, c) -> output (Printf.sprintf "%s = %s.%d" (f_id a) b c)
    | SSA_attribute { object_id; attribute_id; value } -> output (Printf.sprintf "%s.%d = %s" (f_id object_id) attribute_id (f_id value))
    | SSA_internal a -> output (Printf.sprintf "internal %s" a)

    | SSA_return a -> output (Printf.sprintf "return %s" (f_id a))
    | SSA_comment a -> output (Printf.sprintf "comment %s" a)

    | SSA_stack_slot a -> output (Printf.sprintf "stack_slot %s" (f_id a))
    | SSA_store (a, b) -> output (Printf.sprintf "store %s <- %s" (f_id a) (f_id b))
    | SSA_load (a, b) -> output (Printf.sprintf "load %s -> %s" (f_id a) (f_id b))

    | SSA_str_eq (a, b, c) -> output (Printf.sprintf "%s = %s == %s" (f_id a) (f_id b) (f_id c))