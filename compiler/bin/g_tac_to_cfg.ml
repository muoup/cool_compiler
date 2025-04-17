open D_tac_data
open Printf
module StringSet = Set.Make(String)

type basic_block = {
  label: string;
  instructions: tac_cmd list;
  mutable successors: string list; 
  mutable predecessors: string list;
}

type cfg = (string, basic_block) Hashtbl.t

let basic_block_counter = ref 0

let string_of_tac_id = function
  | Local i -> Printf.sprintf "L%d" i
  | Temporary i -> Printf.sprintf "T%d" i
  | Attribute i -> Printf.sprintf "A%d" i
  | Parameter i -> Printf.sprintf "P%d" i
  | Self -> "self"

let string_of_tac (cmd : tac_cmd) : string =
  let id = string_of_tac_id in
  let id_list ids = String.concat ", " (List.map id ids) in
  match cmd with
  | TAC_add (a, b, c) -> Printf.sprintf "%s = %s + %s" (id a) (id b) (id c)
  | TAC_sub (a, b, c) -> Printf.sprintf "%s = %s - %s" (id a) (id b) (id c)
  | TAC_mul (a, b, c) -> Printf.sprintf "%s = %s * %s" (id a) (id b) (id c)
  | TAC_div (_, a, b, c) -> Printf.sprintf "%s = %s / %s" (id a) (id b) (id c)

  | TAC_lt (a, b, c) -> Printf.sprintf "%s = %s < %s" (id a) (id b) (id c)
  | TAC_lte (a, b, c) -> Printf.sprintf "%s = %s <= %s" (id a) (id b) (id c)
  | TAC_eq (a, b, c) -> Printf.sprintf "%s = %s == %s" (id a) (id b) (id c)

  | TAC_int (a, n) -> Printf.sprintf "%s = %d" (id a) n
  | TAC_str (a, s) -> Printf.sprintf "%s = \"%s\"" (id a) s
  | TAC_bool (a, b) -> Printf.sprintf "%s = %b" (id a) b
  | TAC_ident (a, b) -> Printf.sprintf "%s = %s" (id a) (id b)

  | TAC_neg (a, b) -> Printf.sprintf "%s = -%s" (id a) (id b)
  | TAC_not (a, b) -> Printf.sprintf "%s = !%s" (id a) (id b)

  | TAC_new (a, cls) -> Printf.sprintf "%s = new %s" (id a) cls
  | TAC_default (a, cls) -> Printf.sprintf "%s = default %s" (id a) cls
  | TAC_isvoid (a, b) -> Printf.sprintf "%s = isvoid %s" (id a) (id b)

  | TAC_call (dst, fn, args) ->
      Printf.sprintf "%s = call %s(%s)" (id dst) fn (id_list args)

  | TAC_dispatch { store; obj; method_id; args; _ } ->
      Printf.sprintf "%s = dispatch %s.%d(%s)" (id store) (id obj) method_id (id_list args)

  | TAC_label lbl -> Printf.sprintf "%s:" lbl
  | TAC_jmp lbl -> Printf.sprintf "goto %s" lbl
  | TAC_bt (cond, lbl) -> Printf.sprintf "if %s goto %s" (id cond) lbl

  | TAC_object (a, cls, tag) -> Printf.sprintf "%s = object %s (tag %d)" (id a) cls tag
  | TAC_attribute { object_id; attribute_id; value } ->
      Printf.sprintf "%s.attr[%d] = %s" (id object_id) attribute_id (id value)

  | TAC_internal s -> Printf.sprintf "<internal: %s>" s

  | TAC_return v -> Printf.sprintf "return %s" (id v)
  | TAC_comment s -> Printf.sprintf "# %s" s

  | TAC_str_eq (a, b, c) -> Printf.sprintf "%s = (%s == %s)" (id a) (id b) (id c)
  | TAC_str_lt (a, b, c) -> Printf.sprintf "%s = (%s < %s)" (id a) (id b) (id c)
  | TAC_str_lte (a, b, c) -> Printf.sprintf "%s = (%s <= %s)" (id a) (id b) (id c)

  | TAC_void_check (line, v, msg) ->
      Printf.sprintf "void_check (line %d): %s (%s)" line (id v) msg

  | TAC_inline_assembly s ->
      Printf.sprintf "asm { %s }" s



let print_cfg cfg =
  Hashtbl.iter (fun label block ->
    Printf.printf "Block %s:\n" label;
    List.iter (fun instr -> Printf.printf "  %s\n" (string_of_tac instr)) block.instructions;
    Printf.printf "  Successors: %s\n" (String.concat ", " block.successors);
    Printf.printf "  Predecessors: %s\n\n" (String.concat ", " block.predecessors);
  ) cfg


let find_leaders (cmds : tac_cmd list) : StringSet.t =
  let leaders = ref StringSet.empty in
  let rec aux prev_is_jump = function
    | [] -> ()
    | cmd :: rest ->
      begin match cmd with
      | TAC_label label ->
          leaders := StringSet.add label !leaders;
          if prev_is_jump then () else aux false rest
      | TAC_jmp label | TAC_bt (_, label) ->
          leaders := StringSet.add label !leaders;
          aux true rest
      | TAC_return _ ->
          aux true rest
      | _ ->
          aux false rest
      end
  in
  aux true cmds;
  !leaders

let new_block_label () =
  let n = !basic_block_counter in
  incr basic_block_counter;
  Printf.sprintf "block_%d" n

let split_into_blocks (cmds : tac_cmd list) (leaders : StringSet.t) : basic_block list =
  let rec aux acc current_block current_label cmds =
    match cmds with
    | [] ->
        if current_block = [] then acc
        else { label = current_label; instructions = List.rev current_block; successors = []; predecessors = [] } :: acc
    | cmd :: rest ->
        match cmd with
        | TAC_label label when StringSet.mem label leaders ->
            let new_block = { label = current_label; instructions = List.rev current_block; successors = []; predecessors = [] } in
            aux (if current_block = [] then acc else new_block :: acc) [] label rest
        | _ ->
            aux acc (cmd :: current_block) current_label rest
  in
  let first_label = new_block_label () in
  List.rev (aux [] [] first_label cmds)
  
  let add_edges (blocks : basic_block list) : unit =
    let label_map = List.fold_left (fun acc block -> Hashtbl.add acc block.label block; acc) (Hashtbl.create 10) blocks in
    let block_list = Array.of_list blocks in
  
    for i = 0 to Array.length block_list - 1 do
      let block = block_list.(i) in
      match List.rev block.instructions with
      | [] -> ()
      | instr :: _ ->
          let succs =
            match instr with
            | TAC_jmp label -> [label]
            | TAC_bt (_, label) ->
                if i + 1 < Array.length block_list then
                  [label; block_list.(i+1).label]
                else [label]
            | TAC_return _ -> []
            | _ ->
                if i + 1 < Array.length block_list then
                  [block_list.(i+1).label]
                else []
          in
          block.successors <- succs;
          List.iter (fun succ_label ->
            match Hashtbl.find_opt label_map succ_label with
            | Some succ_block -> succ_block.predecessors <- block.label :: succ_block.predecessors
            | None -> ()
          ) succs
    done
  
    let build_cfg (tacs : method_tac list) : cfg =
      let cfg = Hashtbl.create 10 in
    
      let populate_cfg (mthd : method_tac) : unit =
        let leaders = find_leaders mthd.commands in
        let blocks = split_into_blocks mthd.commands leaders in
        add_edges blocks;
        List.iter (fun block -> Hashtbl.add cfg block.label block) blocks
      in
    
      List.iter populate_cfg tacs;
      cfg
    
