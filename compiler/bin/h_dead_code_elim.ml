open D_tac_data
open G_tac_to_cfg

let local_dce (block : basic_block) : tac_cmd list = 
  let rec check_instructions (l : tac_cmd list) : tac_cmd list = 
    match l with
    | [] -> []
    | hd :: tl -> (
      match hd with
      | TAC_int (lhs, i) -> (
        hd :: check_instructions tl 
      )
      | TAC_str (lhs, s) -> (
        hd :: check_instructions tl 
      )
      | TAC_bool (lhs, b) -> (
        hd :: check_instructions tl 
      )
      | TAC_ident (lhs, id) -> (
        if p_id lhs = p_id id then check_instructions tl else (
          hd :: check_instructions tl
        ) 
      )  
      | _ -> (
        hd :: check_instructions tl
      )
    )
  in
  check_instructions block.instructions

let dce_method (mthd : method_cfg) : method_cfg = 
  (* Local DCE *)
  Hashtbl.iter (fun id block ->
    let updated_tac_cmds = local_dce block in
    let updated_block = { block with instructions = updated_tac_cmds } in
    Hashtbl.replace mthd.blocks id updated_block
  ) mthd.blocks;

  (* Global DCE *)
  mthd


let eliminate_dead_code (graph : cfg) : cfg = 
  List.map dce_method graph