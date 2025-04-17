open A_util

type tac_id =
  | Local       of int
  | Temporary   of int
  | Attribute   of int
  | Parameter   of int
  | CallSlot    of int
  | IntLit      of int
  | StrLit      of string
  | RAX
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
    | RAX           -> Printf.sprintf "%%rax"
    | CMP     _type -> Printf.sprintf "cmp"
    | Self          -> "self"

let cmp_str (cmp_type : cmp_type) : string =
    match cmp_type with
    | EQ -> "=="
    | NE -> "!="
    | LT -> "<"
    | GT -> ">"
    | LE -> "<="
    | GE -> ">="

(* TODO: Reimplement tac output *)
let output_tac_cmd (f : string -> unit) (cmd : tac_cmd) =
    match cmd with
    | TAC_add (dst, src1, src2) ->
        f (Printf.sprintf "%s <- %s + %s" (f_id dst) (f_id src1) (f_id src2))
    | TAC_sub (dst, src1, src2) ->
        f (Printf.sprintf "%s <- %s - %s" (f_id dst) (f_id src1) (f_id src2))
    | TAC_mul (dst, src1, src2) ->
        f (Printf.sprintf "%s <- %s * %s" (f_id dst) (f_id src1) (f_id src2))
    | TAC_div (line_num, dst, src1, src2) ->
        f (Printf.sprintf "%s <- %s / %s" (f_id dst) (f_id src1) (f_id src2))

    | TAC_lt (dst, src1, src2) ->
        f (Printf.sprintf "%s <- %s < %s" (f_id dst) (f_id src1) (f_id src2))
    | TAC_lte (dst, src1, src2) ->
        f (Printf.sprintf "%s <- %s <= %s" (f_id dst) (f_id src1) (f_id src2))
    | TAC_eq (dst, src1, src2) ->
        f (Printf.sprintf "%s <- %s == %s" (f_id dst) (f_id src1) (f_id src2))

    | TAC_str_lt (dst, src1, src2) ->
        f (Printf.sprintf "%s <- %s < %s" (f_id dst) (f_id src1) (f_id src2))
    | TAC_str_lte (dst, src1, src2) ->
        f (Printf.sprintf "%s <- %s <= %s" (f_id dst) (f_id src1) (f_id src2))
    | TAC_str_eq (dst, src1, src2) ->
        f (Printf.sprintf "%s <- %s == %s" (f_id dst) (f_id src1) (f_id src2))

    | TAC_int (dst, i) ->
        f (Printf.sprintf "%s <- %d" (f_id dst) i)
    | TAC_str (dst, s) ->
        f (Printf.sprintf "%s <- \"%s\"" (f_id dst) s)
    | TAC_bool (dst, b) ->
        f (Printf.sprintf "%s <- %b" (f_id dst) b)

    | TAC_ident (dst, src) ->
        f (Printf.sprintf "%s <- %s" (f_id dst) (f_id src))

    | TAC_neg (dst, src) ->
        f (Printf.sprintf "%s <- -%s" (f_id dst) (f_id src))
    | TAC_not (dst, src) ->
        f (Printf.sprintf "%s <- !%s" (f_id dst) (f_id src))

    | TAC_new (dst, cls) ->
        f (Printf.sprintf "%s <- new %s" (f_id dst) cls)
    | TAC_default (dst, cls) ->
        f (Printf.sprintf "%s <- default %s" (f_id dst) cls)
    | TAC_isvoid (dst, src) ->
        f (Printf.sprintf "%s <- isvoid %s" (f_id dst) (f_id src))

    | TAC_call_alloc amt ->
        f (Printf.sprintf "allocate %d stack slots for call" amt)
    | TAC_dispatch { line_number; store; obj; method_id; args } ->
        let args_str = String.concat ", " (List.map f_id args) in
        f (Printf.sprintf "%s <- %s.%d(%s)" (f_id store) (f_id obj) method_id args_str)
    | TAC_call (dst, method_name, args) ->
        let args_str = String.concat ", " (List.map f_id args) in
        f (Printf.sprintf "%s <- %s(%s)" (f_id dst) method_name args_str)

    | TAC_label label ->
        f (Printf.sprintf "\n%s:" label)
    | TAC_jmp label ->
        f (Printf.sprintf "jmp %s" label)
    | TAC_bt (cond, label) ->
        f (Printf.sprintf "if %s goto %s" (f_id cond) label)

    | TAC_cmp (cmp_type, src1, src2) ->
        f (Printf.sprintf "cmp %s %s %s" (f_id src1) (cmp_str cmp_type) (f_id src2))
    | TAC_str_cmp (cmp_type, src1, src2) ->
        f (Printf.sprintf "str_cmp %s %s" (f_id src1) (f_id src2))
    | TAC_set (cmp_type, src) ->
        f (Printf.sprintf "set %s" (f_id src))

    | TAC_object (dst, cls, slot) ->
        f (Printf.sprintf "%s <- %s(%d)" (f_id dst) cls slot)
    | TAC_internal s ->
        f (Printf.sprintf "%s" s)
    | TAC_return src ->
        f (Printf.sprintf "return %s" (f_id src))
    | TAC_comment s ->
        f (Printf.sprintf "/* %s */" s)

    | TAC_void_check (line_num, src, cls) ->
        f (Printf.sprintf "test_error %s %s" (f_id src) cls)
    | TAC_inline_assembly s ->
        f (Printf.sprintf "asm %s" s)