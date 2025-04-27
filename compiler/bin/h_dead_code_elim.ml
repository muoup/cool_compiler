open D_tac_data
open G_tac_to_cfg
open Hashtbl
module IntSet = Set.Make(Int)

module IdSet = Set.Make(struct
  type t = tac_id
  let compare = compare
end)

let rec overwrites_before_usage (id : tac_id) (lst : tac_cmd list) : bool =
match lst with
| [] -> false
| hd :: tl ->
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
  
 

let rec go_to_fixpoint f x =
  let x_new = f x in
  if List.length x = List.length x_new then x_new
  else go_to_fixpoint f x_new

let local_dce (block : basic_block) : tac_cmd list =
  let rec remove_dead = function
    | [] -> []
    | instr :: rest ->
      if is_instruction_dead instr rest then (
        remove_dead rest
      ) else
        instr :: remove_dead rest
  in
  go_to_fixpoint remove_dead block.instructions
    
let use_def (block : basic_block) : IdSet.t * IdSet.t =
  let use = ref IdSet.empty in
  let def = ref IdSet.empty in

  let add_use id =
    if not (IdSet.mem id !def) then
      use := IdSet.add id !use
  in
  let add_def id =
    def := IdSet.add id !def
  in

  List.iter (fun cmd ->
    match cmd with
    | TAC_add (dst, lhs, rhs)
    | TAC_sub (dst, lhs, rhs)
    | TAC_mul (dst, lhs, rhs)
    | TAC_div (_, dst, lhs, rhs)
    | TAC_lt (dst, lhs, rhs)
    | TAC_lte (dst, lhs, rhs)
    | TAC_eq (dst, lhs, rhs)
    | TAC_str_eq (dst, lhs, rhs)
    | TAC_str_lt (dst, lhs, rhs)
    | TAC_str_lte (dst, lhs, rhs) ->
        add_use lhs; add_use rhs; add_def dst

    | TAC_neg (dst, src)
    | TAC_not (dst, src)
    | TAC_isvoid (dst, src) ->
        add_use src; add_def dst

    | TAC_int (dst, _)
    | TAC_str (dst, _)
    | TAC_bool (dst, _)
    | TAC_new (dst, _)
    | TAC_default (dst, _)
    | TAC_object (dst, _, _) ->
        add_def dst

    | TAC_ident (dst, src) ->
        add_use src; add_def dst

    | TAC_call (dst, _, args) ->
        List.iter add_use args;
        add_def dst

    | TAC_dispatch {store; obj; args; _} ->
        add_use obj;
        List.iter add_use args;
        add_def store

    | TAC_attribute {object_id; value; _} ->
        add_use object_id;
        add_use value

    | TAC_return id
    | TAC_bt (id, _)
    | TAC_void_check (_, id, _) ->
        add_use id

    | TAC_label _
    | TAC_jmp _
    | TAC_internal _
    | TAC_comment _
    | TAC_inline_assembly _ -> ()
  ) block.instructions;

  (!use, !def)
  
let liveness_analysis (mthd : method_cfg)
  : (int, IdSet.t) Hashtbl.t * (int, IdSet.t) Hashtbl.t =
  let live_in = Hashtbl.create 10 in
  let live_out = Hashtbl.create 10 in
  let use_map = Hashtbl.create 10 in
  let def_map = Hashtbl.create 10 in

  Hashtbl.iter (fun id block ->
    let use_b, def_b = use_def block in
    Hashtbl.add use_map id use_b;
    Hashtbl.add def_map id def_b;
    Hashtbl.add live_in id IdSet.empty;
    Hashtbl.add live_out id IdSet.empty
  ) mthd.blocks;

  let changed = ref true in
  while !changed do
    changed := false;
    Hashtbl.iter (fun id block ->
      let old_in = Hashtbl.find live_in id in
      let old_out = Hashtbl.find live_out id in

      let out =
        List.fold_left (fun acc succ_id ->
          match Hashtbl.find_opt live_in succ_id with
          | Some live_in_succ -> IdSet.union acc live_in_succ
          | None -> acc
        ) IdSet.empty block.successors
      in

      let use_b = Hashtbl.find use_map id in
      let def_b = Hashtbl.find def_map id in
      let in_ = IdSet.union use_b (IdSet.diff out def_b) in

      if not (IdSet.equal in_ old_in) || not (IdSet.equal out old_out) then
        changed := true;

      Hashtbl.replace live_in id in_;
      Hashtbl.replace live_out id out;
    ) mthd.blocks
  done;
  (live_in, live_out)
  
  let global_dce_block (block : basic_block) : basic_block =
    let live = ref IdSet.empty in
    let new_instructions = 
      List.fold_right (fun cmd acc ->
        match cmd with
        (* Critical: always keep control flow operations *)
        | TAC_label _
        | TAC_jmp _
        | TAC_bt _
        | TAC_internal _
        | TAC_comment _
        | TAC_inline_assembly _ ->
            cmd :: acc
  
        (* TAC_return must always be kept *)
        | TAC_return id ->
            live := IdSet.add id !live;
            cmd :: acc
  
        (* Assignments *)
        | TAC_add (dst, lhs, rhs)
        | TAC_sub (dst, lhs, rhs)
        | TAC_mul (dst, lhs, rhs)
        | TAC_lt (dst, lhs, rhs)
        | TAC_lte (dst, lhs, rhs)
        | TAC_eq (dst, lhs, rhs)
        | TAC_str_eq (dst, lhs, rhs)
        | TAC_str_lt (dst, lhs, rhs)
        | TAC_str_lte (dst, lhs, rhs) ->
            if IdSet.mem dst !live then begin
              live := IdSet.add lhs (IdSet.add rhs !live);
              cmd :: acc
            end else
              acc
  
        | TAC_div (_, dst, lhs, rhs) ->
            if IdSet.mem dst !live then begin
              live := IdSet.add lhs (IdSet.add rhs !live);
              cmd :: acc
            end else
              acc
  
        | TAC_neg (dst, src)
        | TAC_not (dst, src)
        | TAC_isvoid (dst, src)
        | TAC_ident (dst, src) ->
            if IdSet.mem dst !live then begin
              live := IdSet.add src !live;
              cmd :: acc
            end else
              acc
  
        | TAC_int (dst, _)
        | TAC_str (dst, _)
        | TAC_bool (dst, _)
        | TAC_new (dst, _)
        | TAC_default (dst, _)
        | TAC_object (dst, _, _) ->
            if IdSet.mem dst !live then
              cmd :: acc
            else
              acc
  
        | TAC_call (dst, _, args) ->
            if IdSet.mem dst !live then begin
              List.iter (fun arg -> live := IdSet.add arg !live) args;
              cmd :: acc
            end else
              acc
  
        | TAC_dispatch {store = dst; obj; args; _} ->
            if IdSet.mem dst !live then begin
              live := IdSet.add obj !live;
              List.iter (fun arg -> live := IdSet.add arg !live) args;
              cmd :: acc
            end else
              acc
  
        | TAC_attribute {object_id; value; _} ->
            (* Always live because modifying an attribute *)
            live := IdSet.add object_id (IdSet.add value !live);
            cmd :: acc
  
        | TAC_void_check (_, id, _) ->
            live := IdSet.add id !live;
            cmd :: acc
      ) block.instructions []
    in
    { block with instructions = new_instructions }
  
  
    let dce_method (mthd : method_cfg) : method_cfg =
      (* Local DCE *)
      (* Hashtbl.iter (fun id block ->
        let updated_tac_cmds = local_dce block in
        let updated_block = { block with instructions = updated_tac_cmds } in
        Hashtbl.replace mthd.blocks id updated_block
      ) mthd.blocks; *)
    
      (* Global DCE *)
      Hashtbl.iter (fun id block ->
        let new_block = global_dce_block block in
        Hashtbl.replace mthd.blocks id new_block
      ) mthd.blocks;
    
      mthd
    


let eliminate_dead_code (graph : cfg) : cfg = 
  List.map dce_method graph