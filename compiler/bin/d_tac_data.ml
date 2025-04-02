type tac_id = string

type tac_cmd =
  | TAC_add     of tac_id * tac_id * tac_id
  | TAC_sub     of tac_id * tac_id * tac_id
  | TAC_mul     of tac_id * tac_id * tac_id
  | TAC_div     of tac_id * tac_id * tac_id

  | TAC_lt      of tac_id * tac_id * tac_id
  | TAC_lte     of tac_id * tac_id * tac_id
  | TAC_eq      of tac_id * tac_id * tac_id

  | TAC_int     of tac_id * int
  | TAC_str     of tac_id * string
  | TAC_bool    of tac_id * bool
  | TAC_ident   of tac_id * tac_id

  | TAC_neg     of tac_id * tac_id
  | TAC_not     of tac_id * tac_id

  | TAC_new     of tac_id * string
  | TAC_default of tac_id * string
  | TAC_isvoid  of tac_id * tac_id
  | TAC_call    of tac_id * string * tac_id list
  | TAC_dispatch of { store : tac_id; obj : tac_id; obj_type : string; method_name : string; args : tac_id list }

  | TAC_label   of string
  | TAC_jmp     of string
  | TAC_bt      of tac_id * string

  | TAC_object   of tac_id * string * int
  | TAC_attribute of { class_name : string; attribute_name : string; value : tac_id }

  | TAC_internal of string

  | TAC_return  of tac_id
  | TAC_comment of string

type method_tac = {
    class_name: string;
    method_name: string;
    arg_count: int;

    commands: tac_cmd list;
    ids: tac_id list;
}

let print_tac_cmd (output : string -> unit) (cmd : tac_cmd) : unit =
  match cmd with
  | TAC_add     (id, a, b) -> output (Printf.sprintf "%s <- + %s %s" id a b)
  | TAC_sub     (id, a, b) -> output (Printf.sprintf "%s <- - %s %s" id a b)
  | TAC_mul     (id, a, b) -> output (Printf.sprintf "%s <- * %s %s" id a b)
  | TAC_div     (id, a, b) -> output (Printf.sprintf "%s <- / %s %s" id a b)

  | TAC_lt      (id, a, b) -> output (Printf.sprintf "%s <- < %s %s" id a b)
  | TAC_lte     (id, a, b) -> output (Printf.sprintf "%s <- <= %s %s" id a b)
  | TAC_eq      (id, a, b) -> output (Printf.sprintf "%s <- = %s %s" id a b)
  
  | TAC_int     (id, i) -> output (Printf.sprintf "%s <- int %d" id i)
  | TAC_str     (id, s) -> output (Printf.sprintf "%s <- string\n%s" id s)
  | TAC_bool    (id, b) -> output (Printf.sprintf "%s <- bool %b" id b)
  | TAC_ident   (id, s) -> output (Printf.sprintf "%s <- %s" id s)

  | TAC_neg     (id, a) -> output (Printf.sprintf "%s <- ~ %s" id a)
  | TAC_not     (id, a) -> output (Printf.sprintf "%s <- not %s" id a)

  | TAC_new     (id, s) -> output (Printf.sprintf "%s <- new %s" id s)
  | TAC_default (id, s) -> output (Printf.sprintf "%s <- default %s" id s)
  | TAC_isvoid  (id, a) -> output (Printf.sprintf "%s <- isvoid %s" id a)
  | TAC_call    (id, s, args) -> output (Printf.sprintf "%s <- call %s" id (String.concat " " @@ s :: args))

  | TAC_internal id -> output (Printf.sprintf "internal %s" id)  

  | TAC_label     s -> output (Printf.sprintf "label %s" s)
  | TAC_jmp       s -> output (Printf.sprintf "jmp %s" s)
  | TAC_bt      (id, s) -> output (Printf.sprintf "bt %s %s" id s)

  | TAC_return   id -> output (Printf.sprintf "return %s" id)
  | TAC_comment   s -> output (Printf.sprintf "comment %s" s)