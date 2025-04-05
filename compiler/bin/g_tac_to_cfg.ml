open D_tac_data
open Printf

type basic_block = {
  label: string option;
  instructions: tac_cmd list;
  successors: string list; 
}

type cfg = (string, basic_block) Hashtbl.t

type method_cfg = {
  class_name: string;
  method_name: string;
  cfg: cfg;
}

(* TODO: Reimplement CFG output *)
(* let print_cfg (cfg : cfg) =
  Printf.printf "CFG Blocks: %d\n" (Hashtbl.length cfg);

  Hashtbl.iter (fun lbl block ->
    printf "Block: %s\n" (Option.value ~default:"<entry>" block.label);
    List.iter (print_tac_cmd (Printf.printf "%s\n")) block.instructions;
    printf "  Successors: [%s]\n\n" (String.concat ", " block.successors)
  ) cfg *)

let build_cfg (method_tac : method_tac) : method_cfg =
  let cfg = Hashtbl.create 10 in
  let current_block = ref { label = None; instructions = []; successors = [] } in
  let label_map = Hashtbl.create 10 in

  let tac_list = method_tac.commands in

  let add_block () =
    match !current_block.label with
    | Some lbl -> Hashtbl.add cfg lbl !current_block
    | None -> ()
  in

  let rec process cmds =
    match cmds with
    | [] -> add_block ()
    | TAC_label lbl :: rest -> 
      add_block ();
      current_block := { label = Some lbl; instructions = []; successors = [] };
      Hashtbl.add label_map lbl !current_block;
      process rest
    | TAC_jmp lbl :: rest ->
      current_block := { !current_block with instructions = !current_block.instructions @ [TAC_jmp lbl]; successors = [lbl] };
      add_block (); process rest
    | TAC_bt (cond, lbl) :: rest ->
      current_block := { !current_block with instructions = !current_block.instructions @ [TAC_bt (cond, lbl)]; successors = [lbl] };
      add_block (); process rest
    | TAC_return _ as ret :: rest ->
      current_block := { !current_block with instructions = !current_block.instructions @ [ret]; successors = [] };
      add_block (); process rest
    | TAC_comment _ :: rest ->
        process rest
    | instr :: rest ->
      current_block := { !current_block with instructions = !current_block.instructions @ [instr] };
      process rest
  in

  process tac_list;

  Hashtbl.iter (fun lbl block ->
    match block.instructions with
    | TAC_jmp _ :: _ | TAC_return _ :: _ -> ()
    | _ ->
      let next_block = Hashtbl.find_opt label_map lbl in
      match next_block with
      | Some next -> Hashtbl.replace cfg lbl { block with successors = block.successors @ [next.label |> Option.get] }
      | None -> ()
  ) cfg;

  {
    class_name = method_tac.class_name;
    method_name = method_tac.method_name;
    cfg = cfg;
  }
