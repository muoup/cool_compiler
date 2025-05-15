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

  locals : int;
  temps : int;
}

type cfg = method_cfg list

let print_cfg (cfgs : cfg) : unit =
  List.iter (fun mcfg ->
    Printf.printf "Method: %s.%s (args: %d)\n" mcfg.class_name mcfg.method_name mcfg.arg_count;
    Printf.printf "Entry block: %d\n" mcfg.entry_block;
    Hashtbl.iter (fun id block ->
      Printf.printf "  Block %d%s:\n" id (match block.label with Some l -> ":" ^ l | None -> "");
      List.iter (fun instr ->
        output_tac_cmd (Printf.printf "    %s") instr
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

    let is_block_ending = function
    | TAC_jmp _
    | TAC_bt _
    | TAC_return _ -> true
    | _ -> false
  
  let split_into_blocks (commands : tac_cmd list) : (string option * tac_cmd list) list =
    let rec aux acc current current_label = function
      | [] -> List.rev ((current_label, List.rev current) :: acc)
      | TAC_label lbl :: rest ->
          let new_block = (current_label, List.rev current) in
          aux (new_block :: acc) [] (Some lbl) rest
      | cmd :: rest when is_block_ending cmd ->
          let current_block = (current_label, List.rev (cmd :: current)) in
          aux (current_block :: acc) [] None rest
      | cmd :: rest ->
          aux acc (cmd :: current) current_label rest
    in
    aux [] [] None commands
  

  let connect_blocks (blocks : (int, basic_block) Hashtbl.t) : unit =
    let label_to_id = Hashtbl.create 16 in
    Hashtbl.iter (fun id block ->
      match block.label with
      | Some lbl -> Hashtbl.add label_to_id lbl id
      | None -> ()
    ) blocks;
  
    let block_list =
      Hashtbl.fold (fun _ b acc -> b :: acc) blocks []
      |> List.sort (fun a b -> compare a.id b.id)
    in
  
    (* Helper to add successor and predecessor relationship *)
    let add_edge from_id to_id =
      let from_block = Hashtbl.find blocks from_id in
      let to_block = Hashtbl.find blocks to_id in
      if not (List.mem to_id from_block.successors) then
        from_block.successors <- to_id :: from_block.successors;
      if not (List.mem from_id to_block.predecessors) then
        to_block.predecessors <- from_id :: to_block.predecessors
    in
  
    List.iteri (fun i block ->
      let last_cmd_opt =
        match List.rev block.instructions with
        | cmd :: _ -> Some cmd
        | [] -> None
      in
  
      match last_cmd_opt with
      | Some (TAC_jmp label) ->
        (match Hashtbl.find_opt label_to_id label with
         | Some target_id -> add_edge block.id target_id
         | None -> ())
      | Some (TAC_bt (_, label)) ->
        (match Hashtbl.find_opt label_to_id label with
         | Some target_id -> add_edge block.id target_id
         | None -> ());
        (match List.nth_opt block_list (i + 1) with
         | Some next_block -> add_edge block.id next_block.id
         | None -> ())
      | Some (TAC_return _) ->()
      | _ ->
        (* default fallthrough to next block *)
        (match List.nth_opt block_list (i + 1) with
         | Some next_block -> add_edge block.id next_block.id
         | None -> ())
    ) block_list
  

let method_tac_to_cfg (m : method_tac) : method_cfg =
  let blocks = Hashtbl.create 8 in
  let raw_blocks = split_into_blocks m.commands in
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
  { class_name = m.class_name; method_name = m.method_name; arg_count = m.arg_count; entry_block = entry_block_id; blocks; locals = m.locals; temps = m.temps }

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
      | Some lbl -> (TAC_label lbl :: b.instructions)
      | None -> b.instructions
    ) sorted_blocks) in
    { class_name = mcfg.class_name;
      method_name = mcfg.method_name;
      arg_count = mcfg.arg_count;
      commands;
      locals = mcfg.locals;
      temps = mcfg.temps; }
  ) cfgs
  
