type tac_id =
  | Local       of int
  | Temporary   of int
  | Attribute   of int
  | Parameter   of int
  | CallSlot    of int
  | IntLit      of int
  | StrLit      of string
  | CMP         of cmp_type
  | Self

and cmp_type =
  | EQ
  | NE
  | LT
  | GT
  | LE
  | GE

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

  | TAC_internal of string

  | TAC_return  of tac_id
  | TAC_comment of string

  (* Boolean in-lining - seperate nodes to not mess with PA4c1 *)
  | TAC_cmp     of cmp_type * tac_id * tac_id (* Implicitly gets into 'Comparison' *)
  | TAC_str_cmp of cmp_type * tac_id * tac_id (* Implicitly gets into 'Comparison' *)
  | TAC_set     of cmp_type * tac_id          (* Implicitly gets from 'Comparison' *)

  (* Special Internal Nodes *)
  | TAC_str_eq  of tac_id * tac_id * tac_id
  | TAC_str_lt  of tac_id * tac_id * tac_id
  | TAC_str_lte of tac_id * tac_id * tac_id

  (* Since this has to cache the line number, it is a separate node, at least for now *)
  | TAC_call_alloc of int
  | TAC_void_check of int * tac_id * string
  | TAC_inline_assembly of string

type method_tac = {
    class_name: string;
    method_name: string;
    arg_count: int;

    commands: tac_cmd list;

    locals : int;
    temps : int;
}

let f_id (id : tac_id) : string =
    match id with
    | Local i       -> Printf.sprintf "L%d" i
    | Temporary i   -> Printf.sprintf "T%d" i
    | Attribute i   -> Printf.sprintf "A%d" i
    | Parameter i   -> Printf.sprintf "P%d" i
    | CallSlot  i   -> Printf.sprintf "C%d" i
    | IntLit    i   -> Printf.sprintf "$%d" i
    | StrLit    s   -> Printf.sprintf "$%s" s
    | CMP     _type -> Printf.sprintf "CMP"
    | Self          -> "self"

(* TODO: Reimplement tac output *)