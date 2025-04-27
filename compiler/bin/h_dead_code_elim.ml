open D_tac_data
open G_tac_to_cfg
open Hashtbl
open StdLabels
module IntSet = Set.Make(Int)  
module IdSet = Set.Make(struct type t = tac_id let compare = Stdlib.compare end)
(* Is this the cause of gradescope error? *)

(* Global Liveness Analysis *)
type liveness_info = {
  mutable live_in : IdSet.t;
  mutable live_out : IdSet.t;
}


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
    | TAC_call (_, _, args) -> List.exists ~f:((=) id) args
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

let local_dead_code_elim (block : basic_block) : tac_cmd list =
  let rec remove_dead = function
    | [] -> []
    | instr :: rest ->
      if is_instruction_dead instr rest then (
        remove_dead rest
      ) else
        instr :: remove_dead rest
  in
  go_to_fixpoint remove_dead block.instructions
    
    
let uses_and_defs (cmd : tac_cmd) : IdSet.t * IdSet.t =
  match cmd with
  | TAC_add (dst, lhs, rhs)
  | TAC_sub (dst, lhs, rhs)
  | TAC_mul (dst, lhs, rhs)
  | TAC_lt  (dst, lhs, rhs)
  | TAC_lte (dst, lhs, rhs)
  | TAC_eq  (dst, lhs, rhs)
  | TAC_str_eq (dst, lhs, rhs)
  | TAC_str_lt (dst, lhs, rhs)
  | TAC_str_lte (dst, lhs, rhs) ->
      IdSet.of_list [lhs; rhs], IdSet.singleton dst

  | TAC_div (_, dst, lhs, rhs) ->
      IdSet.of_list [lhs; rhs], IdSet.singleton dst

  | TAC_neg (dst, src)
  | TAC_not (dst, src)
  | TAC_isvoid (dst, src)
  | TAC_ident (dst, src) ->
      IdSet.singleton src, IdSet.singleton dst

  | TAC_int (dst, _)
  | TAC_str (dst, _)
  | TAC_bool (dst, _)
  | TAC_new (dst, _)
  | TAC_default (dst, _)
  | TAC_object (dst, _, _) ->
      IdSet.empty, IdSet.singleton dst

  | TAC_call (dst, _, args) ->
      IdSet.of_list args, IdSet.singleton dst

  | TAC_dispatch { store; obj; args; _ } ->
      IdSet.of_list (obj :: args), IdSet.singleton store

  | TAC_attribute { object_id; value; _ } ->
      IdSet.of_list [object_id; value], IdSet.empty

  | TAC_return id ->
      IdSet.singleton id, IdSet.empty

  | TAC_void_check (_, id, _) ->
      IdSet.singleton id, IdSet.empty

  | TAC_bt (cond, _) ->
      IdSet.singleton cond, IdSet.empty

  | TAC_label _
  | TAC_jmp _
  | TAC_comment _
  | TAC_internal _
  | TAC_inline_assembly _ ->
      IdSet.empty, IdSet.empty
;;

(* side effects instructions *)
let is_critical (cmd : tac_cmd) : bool =
  match cmd with
  | TAC_call _
  | TAC_dispatch _
  | TAC_inline_assembly _
  | TAC_void_check _
  | TAC_internal _
  | TAC_attribute _ -> true
  | _ -> false

(* Local dead code elimination on a block *)
let local_dce (block : basic_block) : tac_cmd list =
  let live = ref IdSet.empty in
  List.fold_right
    ~f:(fun cmd acc ->
      let uses, defs = uses_and_defs cmd in
      if (not (IdSet.is_empty defs)) && (IdSet.is_empty (IdSet.inter defs !live)) && (not (is_critical cmd)) then
        acc  (* Dead assignment, not critical, can remove *)
      else begin
        live := IdSet.union (IdSet.diff !live defs) uses;
        cmd :: acc
      end
    )
    ~init:[] block.instructions


let dce_method (mthd : method_cfg) : method_cfg =
  let blocks_in_order = Hashtbl.fold (fun id block acc -> (id, block) :: acc) mthd.blocks [] in
  let blocks_in_order = List.sort ~cmp:(fun (id1, _) (id2, _) -> compare id1 id2) blocks_in_order (* Sorting blocks by id *)
in


  (* Local DCE pass *)
  List.iter ~f:(fun (id, block) ->
    let updated_tac_cmds = local_dce block in
    let updated_block = { block with instructions = updated_tac_cmds } in
    Hashtbl.replace mthd.blocks id updated_block
  ) blocks_in_order;

  (* 2. Build initial empty liveness info *)
  let live_info = Hashtbl.create (Hashtbl.length mthd.blocks) in
  Hashtbl.iter (fun id _ ->
    Hashtbl.add live_info id { live_in = IdSet.empty; live_out = IdSet.empty }
  ) mthd.blocks;

  (* 3. Iteratively solve dataflow equations *)
  let changed = ref true in
  while !changed do
    changed := false;
    Hashtbl.iter (fun id block ->
      let info = Hashtbl.find live_info id in
      let old_in = info.live_in in
      let old_out = info.live_out in

      (* live_out = union of live_in of successors *)
      let new_out =
        List.fold_left
          ~f:(fun acc succ_id ->
            let succ_info = Hashtbl.find live_info succ_id in
            IdSet.union acc succ_info.live_in
          )
          ~init:IdSet.empty
          block.successors
      in
      

      (* live_in = uses ∪ (live_out - defs) *)
      let uses, defs =
    List.fold_left
      ~f:(fun (u, d) cmd ->
        let uses_cmd, defs_cmd = uses_and_defs cmd in
        (IdSet.union u uses_cmd, IdSet.union d defs_cmd)
      )
      ~init:(IdSet.empty, IdSet.empty)
      block.instructions

      in
      let new_in = IdSet.union uses (IdSet.diff new_out defs) in

      if not (IdSet.equal new_in old_in) || not (IdSet.equal new_out old_out) then
        changed := true;

      info.live_in <- new_in;
      info.live_out <- new_out;
    ) mthd.blocks;
  done;

  (* 4. Final global DCE pass *)
  Hashtbl.iter (fun id block ->
    let info = Hashtbl.find live_info id in
    let live = ref info.live_out in
    (* Global final DCE pass *)
  let new_instrs =
  List.fold_right
    ~f:(fun cmd acc ->
      let uses, defs = uses_and_defs cmd in
      if (not (IdSet.is_empty defs)) && (IdSet.is_empty (IdSet.inter defs !live)) && (not (is_critical cmd)) then
        acc
      else begin
        live := IdSet.union (IdSet.diff !live defs) uses;
        cmd :: acc
      end
    )
    ~init:[] block.instructions
    in
    Hashtbl.replace mthd.blocks id { block with instructions = new_instrs }
  ) mthd.blocks;

  mthd

let eliminate_dead_code (graph : cfg) : cfg =
  List.map ~f:dce_method graph
