type tac_id =
  | Local       of int
  | Temporary   of int
  | Attribute   of int
  | Parameter   of int
  | Self

module StringTbl = Hashtbl.Make (struct
    type t = string
    let equal = String.equal
    let hash = Hashtbl.hash
end)

type symbol_table = (tac_id * string) StringTbl.t

type tac_cmd =
  | TAC_add     of tac_id * tac_id * tac_id
  | TAC_sub     of tac_id * tac_id * tac_id
  | TAC_mul     of tac_id * tac_id * tac_id
  | TAC_div     of int * tac_id * tac_id * tac_id

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
  | TAC_dispatch of { line_number : int; store : tac_id; obj : tac_id; method_id : int; args : tac_id list }

  | TAC_label   of string
  | TAC_jmp     of string
  | TAC_bt      of tac_id * string

  | TAC_object   of tac_id * string * int
  | TAC_attribute of { object_id : tac_id; attribute_id : int; value : tac_id }

  | TAC_internal of string

  | TAC_return  of tac_id
  | TAC_comment of string

  (* Special Internal Nodes *)
  | TAC_str_eq  of tac_id * tac_id * tac_id
  | TAC_str_lt  of tac_id * tac_id * tac_id
  | TAC_str_lte of tac_id * tac_id * tac_id

  (* Since this has to cache the line number, it is a separate node, at least for now *)
  | TAC_void_check of int * tac_id * string
  | TAC_inline_assembly of string

type method_tac = {
    class_name: string;
    method_name: string;
    arg_count: int;

    commands: tac_cmd list;
    ids: tac_id list;
}

let f_id (id : tac_id) : string =
    match id with
    | Local i       -> Printf.sprintf "L%d" i
    | Temporary i   -> Printf.sprintf "T%d" i
    | Attribute i   -> Printf.sprintf "A%d" i
    | Parameter i   -> Printf.sprintf "P%d" i
    | Self          -> "self"

let p_id (i : tac_id) : string = 
  match i with
  | Self -> ("self")
  | Local x | Temporary x | Attribute x | Parameter x -> (Printf.sprintf "t$%d" x)

let print_tac_cmd (output : string -> unit) (cmd : tac_cmd) : unit =
  match cmd with
  | TAC_add     (id, a, b) -> output (Printf.sprintf "%s <- + %s %s" (p_id id) (p_id a) (p_id b))
  | TAC_sub     (id, a, b) -> output (Printf.sprintf "%s <- - %s %s" (p_id id) (p_id a) (p_id b))
  | TAC_mul     (id, a, b) -> output (Printf.sprintf "%s <- * %s %s" (p_id id) (p_id a) (p_id b))
  | TAC_div     (_, id, a, b) -> output (Printf.sprintf "%s <- / %s %s" (p_id id) (p_id a) (p_id b))

  | TAC_lt      (id, a, b) -> output (Printf.sprintf "%s <- < %s %s" (p_id id) (p_id a) (p_id b))
  | TAC_lte     (id, a, b) -> output (Printf.sprintf "%s <- <= %s %s" (p_id id) (p_id a) (p_id b))
  | TAC_eq      (id, a, b) -> output (Printf.sprintf "%s <- = %s %s" (p_id id) (p_id a) (p_id b))
  
  | TAC_int     (id, i) -> output (Printf.sprintf "%s <- int %d" (p_id id) i)
  | TAC_str     (id, s) -> output (Printf.sprintf "%s <- string\n%s" (p_id id) s)
  | TAC_bool    (id, b) -> output (Printf.sprintf "%s <- bool %b" (p_id id) b)
  | TAC_ident   (id, s) -> output (Printf.sprintf "%s <- %s" (p_id id) (p_id s))

  | TAC_neg     (id, a) -> output (Printf.sprintf "%s <- ~ %s" (p_id id) (p_id a))
  | TAC_not     (id, a) -> output (Printf.sprintf "%s <- not %s" (p_id id) (p_id a))

  | TAC_new     (id, s) -> output (Printf.sprintf "%s <- new %s" (p_id id) s)
  | TAC_default (id, s) -> output (Printf.sprintf "%s <- default %s" (p_id id) s)
  | TAC_isvoid  (id, a) -> output (Printf.sprintf "%s <- isvoid %s" (p_id id) (p_id a))
  | TAC_call    (id, s, args) -> 
    let str_args = List.map p_id args in 
    output (Printf.sprintf "%s <- call %s"  (p_id id) (String.concat " " @@ s :: str_args))

  | TAC_label     s -> output (Printf.sprintf "label %s" s)
  | TAC_jmp       s -> output (Printf.sprintf "jmp %s" s)
  | TAC_bt      (id, s) -> output (Printf.sprintf "bt %s %s" (p_id id) s)

  | TAC_return   id -> output (Printf.sprintf "return %s"  (p_id id))
  | TAC_comment   s -> output (Printf.sprintf "comment %s" s)

  | x -> output (Printf.sprintf "comment Unimplemented")