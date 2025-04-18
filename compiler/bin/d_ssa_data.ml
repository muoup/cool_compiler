open A_util

type ssa_id =
    | SSA_id of int
    | SSA_param of int
    | SSA_attr of int
    | SSA_self

type ssa_symbol = {
    id : ssa_id;
    _type : string;
}

type ssa_sym_table = ssa_symbol StringTbl.t

type ssa_stmt =
    | SSA_Valueless     of ssa_op
    | SSA_Valued        of ssa_id * ssa_op

and binop_type =
    | SSA_add
    | SSA_sub
    | SSA_mul
    | SSA_div
    | SSA_lt
    | SSA_lte
    | SSA_eq

and unop_type =
    | SSA_neg
    | SSA_not

and cmp_procedure =
    | SSA_str_cmp
    | SSA_dir_cmp
    | SSA_ambiguous_cmp

and phi_entry = {
    id : ssa_id;
    from_block : ssa_block;
}

and ssa_op =
    | SSA_bin_op        of { _type : binop_type; lhs : ssa_id; rhs : ssa_id }
    | SSA_un_op         of { _type : unop_type;  lhs : ssa_id; rhs : ssa_id }

    | SSA_alias         of ssa_id

    | SSA_new           of string
    | SSA_default       of string

    | SSA_is_zero       of { _val : ssa_id }
    | SSA_bt            of { _val : ssa_id; label : string }
    | SSA_jmp           of { label : string }

    | SSA_int           of int
    | SSA_str           of string
    | SSA_static        of string

    | SSA_phi           of { entries : phi_entry list; }

    | SSA_internal      of string
    | SSA_return        of ssa_id

    | SSA_call          of { method_name : string; args : ssa_id list }
    | SSA_dispatch      of { obj : ssa_id; method_id : int; args : ssa_id list }

and ssa_block = {
    label : string;
    stmts : ssa_stmt list;
}

type method_ssa = {
    class_name : string;
    method_name : string;
    arg_count : int;

    blocks : ssa_block list;
}

let f_sid (id : ssa_id) : string =
    match id with
    | SSA_id i -> Printf.sprintf "%%%d" i
    | SSA_param i -> Printf.sprintf "param(%d)" i
    | SSA_attr i -> Printf.sprintf "attr(%d)" i
    | SSA_self -> "self"

let f_binop (op : binop_type) : string =
    match op with
    | SSA_add -> "+"
    | SSA_sub -> "-"
    | SSA_mul -> "*"
    | SSA_div -> "/"

    | SSA_lt -> "<"
    | SSA_lte -> "<="
    | SSA_eq -> "=="

let f_unop (op : unop_type) : string =
    match op with
    | SSA_neg -> "-"
    | SSA_not -> "!"

let val_as_str (_val : ssa_op) : string =
    match _val with
    | SSA_bin_op { _type; lhs; rhs } ->
        Printf.sprintf "%s %s %s" (f_sid lhs) (f_binop _type) (f_sid rhs)
    | SSA_un_op { _type; lhs; rhs } ->
        Printf.sprintf "%s %s %s" (f_sid lhs) (f_unop _type) (f_sid rhs)

    | SSA_alias id ->
        Printf.sprintf "alias %s" (f_sid id)

    | SSA_new s ->
        Printf.sprintf "new %s" s
    | SSA_default s ->
        Printf.sprintf "default %s" s
        
    | SSA_is_zero { _val } ->
        Printf.sprintf "is_zero %s" (f_sid _val)
    | SSA_bt { _val; label } ->
        Printf.sprintf "bt %s %s" (f_sid _val) label
    | SSA_jmp { label } ->
        Printf.sprintf "jmp %s" label

    | SSA_int i ->
        Printf.sprintf "%d" i
    | SSA_str s ->
        Printf.sprintf "'%s'" s
    | SSA_static s ->
        Printf.sprintf "$%s" s

    | SSA_phi { entries } ->
        Printf.sprintf "φ %s"
            (String.concat ", " (List.map (fun { id; from_block } ->
                Printf.sprintf "%s:%s" from_block.label (f_sid id)) entries))

    | SSA_internal id ->
        Printf.sprintf "internal %s" id
    | SSA_return id ->
        Printf.sprintf "return %s" (f_sid id)

    | SSA_call { method_name; args } ->
        Printf.sprintf "call %s(%s)" 
            method_name 
            (String.concat ", " (List.map f_sid args))
    | SSA_dispatch { obj; method_id; args } ->
        Printf.sprintf "dispatch %s(%s)" 
            (f_sid obj) 
            (String.concat ", " (List.map f_sid args))

let print_ssa_stmt (output : string -> unit) (stmt : ssa_stmt) : unit =
    match stmt with
    | SSA_Valueless op ->
        output (val_as_str op)
    | SSA_Valued (id, op) ->
        output (Printf.sprintf "%s = %s" (f_sid id) (val_as_str op))