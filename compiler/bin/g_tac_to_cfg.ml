open D_tac_data
open Printf
module StringSet = Set.Make(String)

type basic_block = {
  id: int;
  label: string option;
  instructions: tac_cmd list;

  mutable predecessors: int list;
  mutable successors: int list;

  class_name: string;
  method_name: string;
  arg_count: int;
}

type cfg = {
  blocks: (int, basic_block) Hashtbl.t;
  label_map: (string, int) Hashtbl.t;
  entry_block: int;
}

let string_of_tac_id = function
  | Local i     -> Printf.sprintf "Local(%d)" i
  | Temporary i -> Printf.sprintf "Temp(%d)" i
  | Attribute i -> Printf.sprintf "Attr(%d)" i
  | Parameter i -> Printf.sprintf "Param(%d)" i
  | Self        -> "Self"

let string_of_tac_cmd = function
  | TAC_add (x, y, z)     -> Printf.sprintf "%s := %s + %s" (string_of_tac_id x) (string_of_tac_id y) (string_of_tac_id z)
  | TAC_sub (x, y, z)     -> Printf.sprintf "%s := %s - %s" (string_of_tac_id x) (string_of_tac_id y) (string_of_tac_id z)
  | TAC_mul (x, y, z)     -> Printf.sprintf "%s := %s * %s" (string_of_tac_id x) (string_of_tac_id y) (string_of_tac_id z)
  | TAC_div (_, x, y, z)  -> Printf.sprintf "%s := %s / %s" (string_of_tac_id x) (string_of_tac_id y) (string_of_tac_id z)

  | TAC_lt (x, y, z)      -> Printf.sprintf "%s := %s < %s" (string_of_tac_id x) (string_of_tac_id y) (string_of_tac_id z)
  | TAC_lte (x, y, z)     -> Printf.sprintf "%s := %s <= %s" (string_of_tac_id x) (string_of_tac_id y) (string_of_tac_id z)
  | TAC_eq (x, y, z)      -> Printf.sprintf "%s := %s == %s" (string_of_tac_id x) (string_of_tac_id y) (string_of_tac_id z)

  | TAC_int (x, i)        -> Printf.sprintf "%s := %d" (string_of_tac_id x) i
  | TAC_str (x, s)        -> Printf.sprintf "%s := \"%s\"" (string_of_tac_id x) s
  | TAC_bool (x, b)       -> Printf.sprintf "%s := %b" (string_of_tac_id x) b
  | TAC_ident (x, y)      -> Printf.sprintf "%s := %s" (string_of_tac_id x) (string_of_tac_id y)

  | TAC_neg (x, y)        -> Printf.sprintf "%s := -%s" (string_of_tac_id x) (string_of_tac_id y)
  | TAC_not (x, y)        -> Printf.sprintf "%s := !%s" (string_of_tac_id x) (string_of_tac_id y)

  | TAC_new (x, cls)      -> Printf.sprintf "%s := new %s" (string_of_tac_id x) cls
  | TAC_default (x, cls)  -> Printf.sprintf "%s := default %s" (string_of_tac_id x) cls
  | TAC_isvoid (x, y)     -> Printf.sprintf "%s := isvoid %s" (string_of_tac_id x) (string_of_tac_id y)
  | TAC_call (x, f, args) ->
    let arg_str = String.concat ", " (List.map string_of_tac_id args) in
    Printf.sprintf "%s := call %s(%s)" (string_of_tac_id x) f arg_str

  | TAC_dispatch { line_number; store; obj; method_id; args } ->
    let arg_str = String.concat ", " (List.map string_of_tac_id args) in
    Printf.sprintf "%s := dispatch line %d, obj=%s, method#%d(%s)"
      (string_of_tac_id store) line_number (string_of_tac_id obj) method_id arg_str

  | TAC_label l           -> Printf.sprintf "Label %s:" l
  | TAC_jmp l             -> Printf.sprintf "jmp %s" l
  | TAC_bt (cond, l)      -> Printf.sprintf "if %s goto %s" (string_of_tac_id cond) l

  | TAC_object (x, cls, _) -> Printf.sprintf "%s := object(%s)" (string_of_tac_id x) cls
  | TAC_attribute { object_id; attribute_id; value } ->
    Printf.sprintf "attr %d of %s := %s"
      attribute_id (string_of_tac_id object_id) (string_of_tac_id value)

  | TAC_internal s        -> Printf.sprintf "internal: %s" s
  | TAC_inline_assembly s -> Printf.sprintf "asm: %s" s
  | TAC_void_check (line, x, msg) ->
    Printf.sprintf "void check line %d on %s: \"%s\"" line (string_of_tac_id x) msg

  | TAC_return x          -> Printf.sprintf "return %s" (string_of_tac_id x)
  | TAC_comment s         -> Printf.sprintf "# %s" s

  | TAC_str_eq (x, y, z)
  | TAC_str_lt (x, y, z)
  | TAC_str_lte (x, y, z) ->
    Printf.sprintf "%s := str-op(%s, %s)" (string_of_tac_id x) (string_of_tac_id y) (string_of_tac_id z)

let print_cfg (cfg : cfg) : unit =
  Printf.printf "CFG Entry Block: %d\n" cfg.entry_block;
  Hashtbl.iter (fun id block ->
    Printf.printf "\nBlock ID: %d\n" block.id;
    (match block.label with
     | Some l -> Printf.printf "Label: %s\n" l
     | None -> ());
    Printf.printf "Class.Method: %s.%s\n" block.class_name block.method_name;
    Printf.printf "Predecessors: [%s]\n"
      (String.concat ", " (List.map string_of_int block.predecessors));
    Printf.printf "Successors:   [%s]\n"
      (String.concat ", " (List.map string_of_int block.successors));
    Printf.printf "Instructions:\n";
    List.iter (fun cmd ->
      Printf.printf "  %s\n" (string_of_tac_cmd cmd)
    ) block.instructions;
  ) cfg.blocks


let rec collect_ids_from_cmd (cmd : tac_cmd) : tac_id list =
  match cmd with
  | TAC_add (x, y, z)
  | TAC_sub (x, y, z)
  | TAC_mul (x, y, z)
  | TAC_lt  (x, y, z)
  | TAC_lte (x, y, z)
  | TAC_eq  (x, y, z)
  | TAC_str_eq (x, y, z)
  | TAC_str_lt (x, y, z)
  | TAC_str_lte (x, y, z) -> [x; y; z]

  | TAC_div (_, x, y, z) -> [x; y; z]
  | TAC_int (x, _)
  | TAC_str (x, _)
  | TAC_bool (x, _)
  | TAC_new (x, _)
  | TAC_default (x, _)
  | TAC_isvoid (x, _)
  | TAC_return x
  | TAC_neg (x, _)
  | TAC_not (x, _)
  | TAC_ident (x, _) -> [x]

  | TAC_call (x, _, args) -> x :: args
  | TAC_dispatch { store; obj; args; _ } -> store :: obj :: args
  | TAC_object (x, _, _)
  | TAC_void_check (_, x, _) -> [x]

  | TAC_attribute { object_id; value; _ } -> [object_id; value]

  | TAC_bt (x, _) -> [x]
  | TAC_jmp _
  | TAC_label _
  | TAC_internal _
  | TAC_inline_assembly _
  | TAC_comment _ -> []

let collect_ids (cmds : tac_cmd list) : tac_id list =
  let all_ids = List.flatten (List.map collect_ids_from_cmd cmds) in
  (* Remove duplicates *)
  let module IdSet = Set.Make(struct
    type t = tac_id
    let compare = compare
  end) in
  IdSet.elements (List.fold_left (fun acc id -> IdSet.add id acc) IdSet.empty all_ids)

let build_cfg (methods : method_tac list) : cfg =
  let blocks = Hashtbl.create 64 in
  let label_map = Hashtbl.create 64 in
  let block_id = ref 0 in

  let make_block ~label ~cmds ~cls ~mth ~args =
    let id = !block_id in
    incr block_id;
    let b = {
      id;
      label;
      instructions = cmds;
      predecessors = [];
      successors = [];
      class_name = cls;
      method_name = mth;
      arg_count = args;
    } in
    Hashtbl.add blocks id b;
    (match label with Some l -> Hashtbl.add label_map l id | None -> ());
    id
  in

  let rec split_to_blocks acc current cmds cls mth args =
    match cmds with
    | [] ->
      let id = make_block ~label:None ~cmds:(List.rev current) ~cls ~mth ~args in
      List.rev (id :: acc)
    | TAC_label l :: rest ->
      let acc =
        if current <> [] then
          let id = make_block ~label:None ~cmds:(List.rev current) ~cls ~mth ~args in
          id :: acc
        else acc
      in
      let id = make_block ~label:(Some l) ~cmds:[] ~cls ~mth ~args in
      split_to_blocks (id :: acc) [] rest cls mth args
    | instr :: rest ->
      let current = instr :: current in
      match instr with
      | TAC_jmp _ | TAC_bt _ | TAC_return _ ->
        let id = make_block ~label:None ~cmds:(List.rev current) ~cls ~mth ~args in
        split_to_blocks (id :: acc) [] rest cls mth args
      | _ -> split_to_blocks acc current rest cls mth args
  in

  (* build all basic blocks *)
  List.iter (fun m ->
    ignore (split_to_blocks [] [] m.commands m.class_name m.method_name m.arg_count)
  ) methods;

  (* connect successors and predecessors *)
  Hashtbl.iter (fun _ block ->
    match List.rev block.instructions with
    | [] -> ()
    | last :: _ -> (
        match last with
        | TAC_jmp l ->
          let target = Hashtbl.find label_map l in
          block.successors <- [target];
          let t = Hashtbl.find blocks target in
          t.predecessors <- block.id :: t.predecessors
        | TAC_bt (_, l) ->
          let target = Hashtbl.find label_map l in
          let fallthrough = block.id + 1 in
          block.successors <- [target; fallthrough];
          let t1 = Hashtbl.find blocks target in
          let t2 = Hashtbl.find_opt blocks fallthrough in
          t1.predecessors <- block.id :: t1.predecessors;
          (match t2 with Some b2 -> b2.predecessors <- block.id :: b2.predecessors | None -> ())
        | TAC_return _ -> ()
        | _ ->
          let fallthrough = block.id + 1 in
          match Hashtbl.find_opt blocks fallthrough with
          | Some b2 ->
            block.successors <- [fallthrough];
            b2.predecessors <- block.id :: b2.predecessors
          | None -> ()
      )
  ) blocks;

  (* determine entry block *)
  let entry_block =
    Hashtbl.fold (fun _ b acc ->
      if b.class_name = "Main" && b.method_name = "main" then b.id else acc
    ) blocks (-1)
  in

  { blocks; label_map; entry_block }

    
let cfg_to_method_tac (g : cfg) : method_tac list =
  let grouped = Hashtbl.create 16 in

  Hashtbl.iter (fun _ b ->
    let key = (b.class_name, b.method_name) in
    let existing = Hashtbl.find_opt grouped key |> Option.value ~default:[] in
    Hashtbl.replace grouped key (b :: existing)
  ) g.blocks;

  Hashtbl.fold (fun (cls, mth) blocks acc ->
    let sorted_blocks = List.sort (fun a b -> compare a.id b.id) blocks in
    let commands = List.flatten (List.map (fun b ->
      match b.label with
      | Some l -> TAC_label l :: b.instructions
      | None -> b.instructions
    ) sorted_blocks) in
    let ids = collect_ids commands in
    let arg_count = (List.hd sorted_blocks).arg_count in
    { class_name = cls; method_name = mth; arg_count; commands; ids } :: acc
  ) grouped []

  
