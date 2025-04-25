open D_tac_data
open G_tac_to_cfg

let rec overwrites_before_usage (id : tac_id) (lst : tac_cmd list) : bool =
match lst with
| [] -> false
| hd :: tl ->
  Printf.printf "Checking if id %s is overwritten or used by instruction %s\n"
    (p_id id) (string_of_tac_cmd hd);
  let is_used = match hd with
    | TAC_add (_, a, b)
    | TAC_sub (_, a, b)
    | TAC_mul (_, a, b)
    | TAC_div (_, _, a, b)
    | TAC_lt  (_, a, b)
    | TAC_lte (_, a, b)
    | TAC_eq  (_, a, b) -> id = a || id = b
    | TAC_neg (_, a)
    | TAC_not (_, a)
    | TAC_isvoid (_, a)
    | TAC_ident (_, a) -> id = a
    | TAC_call (_, _, args) -> List.exists ((=) id) args
    | _ -> false
  in
  if is_used then false
  else
    let is_overwritten = match hd with
      | TAC_add (lhs, _, _)
      | TAC_sub (lhs, _, _)
      | TAC_mul (lhs, _, _)
      | TAC_div (_, lhs, _, _)
      | TAC_lt  (lhs, _, _)
      | TAC_lte (lhs, _, _)
      | TAC_eq  (lhs, _, _)
      | TAC_neg (lhs, _)
      | TAC_not (lhs, _)
      | TAC_isvoid (lhs, _)
      | TAC_int (lhs, _)
      | TAC_str (lhs, _)
      | TAC_bool (lhs, _)
      | TAC_ident (lhs, _)
      | TAC_new (lhs, _)
      | TAC_default (lhs, _) -> id = lhs
      | _ -> false
    in
    if is_overwritten then true else overwrites_before_usage id tl



  let is_instruction_dead (instruction : tac_cmd) (rest : tac_cmd list) : bool =
    Printf.printf "Checking if instruction is dead: %s\n" (string_of_tac_cmd instruction);
    match instruction with
    | TAC_int (lhs, _)
    | TAC_bool (lhs, _)
    | TAC_str (lhs, _)
    | TAC_ident (lhs, _)
    | TAC_add (lhs, _, _)
    | TAC_sub (lhs, _, _)
    | TAC_mul (lhs, _, _)
    | TAC_div (_, lhs, _, _)
    | TAC_lt (lhs, _, _)
    | TAC_lte (lhs, _, _)
    | TAC_eq (lhs, _, _)
    | TAC_neg (lhs, _)
    | TAC_not (lhs, _)
    | TAC_isvoid (lhs, _)
    | TAC_default (lhs, _) ->
      overwrites_before_usage lhs rest
    | _ -> false
  
 

let local_dce (block : basic_block) : tac_cmd list =
  Printf.printf "Processing block %d for local DCE\n" block.id;
  let rec remove_dead = function
    | [] -> []
    | instr :: rest ->
      if is_instruction_dead instr rest then
        (Printf.printf "Removing dead instruction: %s\n" (string_of_tac_cmd instr);
          remove_dead rest)
      else
        instr :: remove_dead rest
  in
  remove_dead block.instructions
  

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