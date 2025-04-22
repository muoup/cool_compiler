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
  | Self -> ("")
  | Local x | Temporary x | Attribute x | Parameter x -> (Printf.sprintf "t$%d" x)

let rec stringify_args (l : tac_id list) : string list =
  match l with 
  | [] -> []
  | hd :: tl -> (
    match hd with
      | Self -> (stringify_args tl)
      | x -> p_id x :: stringify_args tl 
  )

let get_main_main (methods : method_tac list) : method_tac list =
  List.filter (fun m -> (m.class_name = "Main" && m.method_name = "Main.main")) methods

let all_relevant_tac_commands (mt : method_tac list) : tac_cmd list =
    List.concat_map (fun m -> m.commands) mt

let remove_main_prefix (s : string) : string =
  let prefix = "Main." in
  if String.length s >= String.length prefix && String.sub s 0 (String.length prefix) = prefix then
    String.sub s (String.length prefix) (String.length s - String.length prefix)
  else
    s


(* I'm aware this "do we need a backslash or not" malarkey is hideous. It's only needed for PA4C1 *)
let remove_extra_backslashes (s : string) : string =
  let len = String.length s in
  let result = ref "" in
  let rec loop i =
    if i >= len then ()
    else if i + 1 < len && s.[i] = '\\' && s.[i + 1] = '\\' then (
      result := !result ^ "\\";
      loop (i + 2)
    )
    else (
      result := !result ^ String.make 1 s.[i];
      loop (i + 1)
    )
  in
  loop 0;
  !result

(* This is ugly code. It's only needed for pa4cl and I'm doing it like this because I want all of the
necessary tac manipulation for correct output to not occur in the real assembly. *)
let print_tac_cmd_for_pa4c1 (output : string -> unit) (cmd : tac_cmd) : unit =
  match cmd with
  | TAC_add     (id, a, b) -> output (Printf.sprintf "%s <- + %s %s" (p_id id) (p_id a) (p_id b))
  | TAC_sub     (id, a, b) -> output (Printf.sprintf "%s <- - %s %s" (p_id id) (p_id a) (p_id b))
  | TAC_mul     (id, a, b) -> output (Printf.sprintf "%s <- * %s %s" (p_id id) (p_id a) (p_id b))
  | TAC_div     (_, id, a, b) -> output (Printf.sprintf "%s <- / %s %s" (p_id id) (p_id a) (p_id b))

  | TAC_lt      (id, a, b) -> output (Printf.sprintf "%s <- < %s %s" (p_id id) (p_id a) (p_id b))
  | TAC_lte     (id, a, b) -> output (Printf.sprintf "%s <- <= %s %s" (p_id id) (p_id a) (p_id b))
  | TAC_eq      (id, a, b) -> output (Printf.sprintf "%s <- = %s %s" (p_id id) (p_id a) (p_id b))
  
  | TAC_int     (id, i) -> output (Printf.sprintf "%s <- int %d" (p_id id) i)
  | TAC_str     (id, s) -> output (Printf.sprintf "%s <- string\n%s" (p_id id) (remove_extra_backslashes s))
  | TAC_bool    (id, b) -> output (Printf.sprintf "%s <- bool %b" (p_id id) b)
  | TAC_ident   (id, s) -> output (Printf.sprintf "%s <- %s" (p_id id) (p_id s))

  | TAC_neg     (id, a) -> output (Printf.sprintf "%s <- ~ %s" (p_id id) (p_id a))
  | TAC_not     (id, a) -> output (Printf.sprintf "%s <- not %s" (p_id id) (p_id a))

  | TAC_new     (id, s) -> output (Printf.sprintf "%s <- new %s" (p_id id) s)
  | TAC_default (id, s) -> output (Printf.sprintf "%s <- default %s" (p_id id) s)
  | TAC_isvoid  (id, a) -> output (Printf.sprintf "%s <- isvoid %s" (p_id id) (p_id a))
  | TAC_call    (id, s, args) -> 
    (* This is a pretty terrible hack because it introduces an unnecessary assignment DCE won't be able
    to check. But if I do DCE right then in the final PA4 it will work fully so maybe this is fine? *)
    if s = "unlift_int" then 
    output (Printf.sprintf "%s <- %s"  (p_id id) (List.hd (List.rev (stringify_args args))))
    else
    output (Printf.sprintf "%s <- call %s"  (p_id id) (String.concat " " @@ remove_main_prefix s :: (stringify_args args)))

  | TAC_label     s -> output (Printf.sprintf "label %s" s)
  | TAC_jmp       s -> output (Printf.sprintf "jmp %s" s)
  | TAC_bt      (id, s) -> output (Printf.sprintf "bt %s %s" (p_id id) s)

  | TAC_return   id -> output (Printf.sprintf "return %s"  (p_id id))
  | TAC_comment   s -> output (Printf.sprintf "comment %s" s)

  | x -> ()