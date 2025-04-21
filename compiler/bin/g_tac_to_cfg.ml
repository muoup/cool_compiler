open D_tac_data
open Printf

type basic_block = {
  id : int;
  label : string option;
  class_name : string;
  method_name : string;
  instructions : tac_cmd list;
  mutable successors : int list;
  mutable predecessors : int list;
}

type method_cfg = {
  class_name : string;
  method_name : string;
  arg_count : int;
  entry_block : int;
  blocks : (int, basic_block) Hashtbl.t;
  ids : tac_id list;
}

type cfg = method_cfg list
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

let print_cfg (cfgs : cfg) : unit =
  List.iter (fun mcfg ->
    Printf.printf "Method: %s.%s (args: %d)\n" mcfg.class_name mcfg.method_name mcfg.arg_count;
    Printf.printf "Entry block: %d\n" mcfg.entry_block;
    Hashtbl.iter (fun id block ->
      Printf.printf "  Block %d%s:\n" id (match block.label with Some l -> ":" ^ l | None -> "");
      List.iter (fun instr ->
        Printf.printf "    %s\n" (string_of_tac_cmd instr)
      ) block.instructions;
      Printf.printf "    Successors: [%s]\n" (String.concat ", " (List.map string_of_int block.successors));
      Printf.printf "    Predecessors: [%s]\n" (String.concat ", " (List.map string_of_int block.predecessors));
    ) mcfg.blocks;
    print_endline ""
  ) cfgs
    

let fresh_block_id =
  let counter = ref 0 in
  fun () ->
    let id = !counter in
    incr counter;
    id

let split_into_blocks (commands : tac_cmd list) : (string option * tac_cmd list) list =
  let rec aux current acc current_label = function
    | [] -> List.rev ((current_label, List.rev current) :: acc)
    | TAC_label lbl :: rest ->
      let new_block = (current_label, List.rev current) in
      aux [] (new_block :: acc) (Some lbl) rest
    | instr :: rest ->
      aux (instr :: current) acc current_label rest
  in
  aux [] [] None commands

let connect_blocks (blocks : (int, basic_block) Hashtbl.t) : unit =
  let label_to_id = Hashtbl.create 16 in
  Hashtbl.iter (fun id block ->
    match block.label with
    | Some lbl -> Hashtbl.add label_to_id lbl id
    | None -> ()
  ) blocks;

  let block_list = Hashtbl.fold (fun _ b acc -> b :: acc) blocks [] |> List.sort (fun a b -> compare a.id b.id) in

  List.iteri (fun i block ->
    let last_cmd = match List.rev block.instructions with
      | cmd :: _ -> Some cmd
      | [] -> None
    in
    let add_succ pid sid =
      if not (List.mem sid (Hashtbl.find blocks pid).successors) then
        (Hashtbl.find blocks pid).successors <- sid :: (Hashtbl.find blocks pid).successors;
      if not (List.mem pid (Hashtbl.find blocks sid).predecessors) then
        (Hashtbl.find blocks sid).predecessors <- pid :: (Hashtbl.find blocks sid).predecessors
    in
    match last_cmd with
    | Some (TAC_jmp label) ->
      if Hashtbl.mem label_to_id label then
        add_succ block.id (Hashtbl.find label_to_id label)
    | Some (TAC_bt (_, label)) ->
      if Hashtbl.mem label_to_id label then
        add_succ block.id (Hashtbl.find label_to_id label);
      (match List.nth_opt block_list (i+1) with
       | Some next -> add_succ block.id next.id
       | None -> ())
    | Some (TAC_return _) -> ()
    | _ ->
      (match List.nth_opt block_list (i+1) with
       | Some next -> add_succ block.id next.id
       | None -> ())
  ) block_list

let method_tac_to_cfg (m : method_tac) : method_cfg =
  let blocks = Hashtbl.create 8 in
  let raw_blocks = split_into_blocks m.tac_commands in
  let entry_block_id = fresh_block_id () in

  let _ =
    List.fold_left (fun last_id (label, cmds) ->
      let id = if label = None && last_id = None then entry_block_id else fresh_block_id () in
      let block = {
        id;
        label;
        class_name = m.class_name;
        method_name = m.method_name;
        instructions = cmds;
        successors = [];
        predecessors = [];
      } in
      Hashtbl.add blocks id block;
      Some id
    ) None raw_blocks
  in
  connect_blocks blocks;
  { class_name = m.class_name; method_name = m.method_name; arg_count = m.arg_count; entry_block = entry_block_id; blocks; ids = m.ids }

let build_cfg (methods : method_tac list) : cfg =
  List.map method_tac_to_cfg methods

  let cfg_to_method_tac_list (cfgs : cfg) : method_tac list =
    let block_sort blocks =
      blocks |> Hashtbl.to_seq |> List.of_seq |> List.sort (fun (i1, _) (i2, _) -> compare i1 i2) |> List.map snd
    in
    List.map (fun mcfg ->
      let sorted_blocks = block_sort mcfg.blocks in
      let commands = List.flatten (List.map (fun b ->
        match b.label with
        | Some lbl -> TAC_label lbl :: b.instructions
        | None -> b.instructions
      ) sorted_blocks) in
      { class_name = mcfg.class_name;
        method_name = mcfg.method_name;
        arg_count = mcfg.arg_count;
        tac_commands = commands;
        ids = mcfg.ids }
    ) cfgs
  
