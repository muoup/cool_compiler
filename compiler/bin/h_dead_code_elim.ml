open D_tac_data
open G_tac_to_cfg

let rec overwrites_before_usage (id : tac_id) (lst : tac_cmd list) : bool = 
  match lst with 
  | [] -> false
  | hd :: tl -> (
      match hd with 
      | TAC_add     (left, a, b)
      | TAC_sub     (left, a, b)
      | TAC_mul     (left, a, b)
      | TAC_div     (_, left, a, b)
      | TAC_lt      (left, a, b)
      | TAC_lte     (left, a, b)
      | TAC_eq      (left, a, b) -> (
        if (id = left && id != a && id != b) then true 
        else if (id = a || id = b) then false
        else overwrites_before_usage id tl
    )
      | TAC_int     (left, _)
      | TAC_str     (left, _)
      | TAC_new     (left, _)
      | TAC_default (left, _)
      | TAC_bool    (left, _) -> (
        if id = left then true 
        else overwrites_before_usage id tl
    )
      | TAC_ident   (left, a)
      | TAC_neg     (left, a)
      | TAC_isvoid  (left, a)
      | TAC_not     (left, a) -> (
        if id = left && id != a then true 
        else if id = a then false
        else overwrites_before_usage id tl
    )
      | TAC_call    (left, s, args) -> false (* no inlining yet *)
      | TAC_comment   s -> overwrites_before_usage id tl
      (* everything else shouldn't be reached or destroys all liveness info *)
      | x -> false
  )


let is_instruction_dead (instruction : tac_cmd) (lst : tac_cmd list) : bool = 
  match instruction with 
  | TAC_int (lhs, _)
  | TAC_bool (lhs, _)
  | TAC_str (lhs, _) 
  | TAC_ident (lhs, _) 
  | TAC_add (lhs, _,  _)
  | TAC_sub (lhs, _,  _)
  | TAC_mul (lhs, _,  _)
  | TAC_div (_, lhs, _,  _)
  | TAC_lt (lhs, _,  _)
  | TAC_lte (lhs, _,  _)
  | TAC_eq (lhs, _,  _)
  | TAC_neg (lhs, _)
  | TAC_not (lhs, _)
  (* Not sure about new or default yet - will this break some assembly code? *)
  (* | TAC_new (lhs, _) *)
  (* | TAC_default (lhs,  _) *)
  | TAC_isvoid (lhs,  _) -> (
    if overwrites_before_usage lhs lst then true else false
  )
  | _ -> false 
 

let local_dce (block : basic_block) : tac_cmd list = 
  let rec check_instructions (l : tac_cmd list) : tac_cmd list = 
    match l with
    | [] -> []
    | hd :: tl -> (
      match hd with
      | TAC_int (lhs, _) | TAC_bool(lhs, _) | TAC_str (lhs, _) | TAC_ident (lhs, _) -> (
          if is_instruction_dead hd tl then check_instructions tl 
          else hd :: check_instructions tl
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

  (* Blocks with no sucessors are dead *)


  (* Global DCE *)
  mthd


let eliminate_dead_code (graph : cfg) : cfg = 
  List.map dce_method graph