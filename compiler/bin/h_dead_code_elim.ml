open B_ast
open C_parser_data
open D_tac_data
open G_tac_to_cfg
open Hashtbl
module IntSet = Set.Make(Int)  
module IdSet = Set.Make(struct
  type t = tac_id
  let compare = compare
end)

type live_info = {
  mutable live_in : IdSet.t;
  mutable live_out : IdSet.t;
}

type class_and_method = {
  class_name : string;
  method_name : string;
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
  
  

  let dce_method (mthd : method_cfg) : method_cfg =
    let live_info = Hashtbl.create 16 in
  
    Hashtbl.iter (fun id _ ->
      Hashtbl.add live_info id { live_in = IdSet.empty; live_out = IdSet.empty }
    ) mthd.blocks;
  
    let uses_and_defs (cmd : tac_cmd) : IdSet.t * IdSet.t =
      match cmd with
      | TAC_add (dst, lhs, rhs)
      | TAC_sub (dst, lhs, rhs)
      | TAC_mul (dst, lhs, rhs)
      | TAC_lt (dst, lhs, rhs)
      | TAC_lte (dst, lhs, rhs)
      | TAC_eq (dst, lhs, rhs)
      | TAC_str_eq (dst, lhs, rhs)
      | TAC_str_lt (dst, lhs, rhs)
      | TAC_str_lte (dst, lhs, rhs) ->
          (IdSet.of_list [lhs; rhs], IdSet.singleton dst)
      | TAC_div (_, dst, lhs, rhs) ->
          (IdSet.of_list [lhs; rhs], IdSet.singleton dst)
      | TAC_neg (dst, src)
      | TAC_not (dst, src)
      | TAC_isvoid (dst, src)
      | TAC_ident (dst, src) ->
          (IdSet.singleton src, IdSet.singleton dst)
      | TAC_int (dst, _)
      | TAC_str (dst, _)
      | TAC_bool (dst, _)
      | TAC_new (dst, _)
      | TAC_default (dst, _)
      | TAC_object (dst, _, _) ->
          (IdSet.empty, IdSet.singleton dst)
      | TAC_call (dst, _, args) ->
          (IdSet.of_list args, IdSet.singleton dst)
      | TAC_dispatch { store; obj; args; _ } ->
          (IdSet.add obj (IdSet.of_list args), IdSet.singleton store)
      | TAC_attribute { object_id; value; _ } ->
          (IdSet.of_list [object_id; value], IdSet.empty)
      | TAC_void_check (_, id, _) ->
          (IdSet.singleton id, IdSet.empty)
      | TAC_return id ->
          (IdSet.singleton id, IdSet.empty)
      | TAC_bt (id, _) ->
          (IdSet.singleton id, IdSet.empty)
      | TAC_jmp _
      | TAC_label _
      | TAC_comment _
      | TAC_internal _
      | TAC_inline_assembly _ ->
          (IdSet.empty, IdSet.empty)
    in
  
    let side_effecting_uses (cmd : tac_cmd) : IdSet.t =
      match cmd with
      | TAC_dispatch { obj; args; _ } ->
          IdSet.of_list (obj :: args)
      | TAC_call (_, _, args) ->
          IdSet.of_list args
      | TAC_void_check (_, id, _) ->
          IdSet.singleton id
      | TAC_div (_, _, lhs, rhs) ->
          IdSet.of_list [lhs; rhs]
      | TAC_attribute { object_id; value; _ } ->
          IdSet.of_list [object_id; value]
      | _ -> IdSet.empty
    in
  
    let is_critical (cmd : tac_cmd) : bool =
      match cmd with
      | TAC_ident _ (* yes i'm aware this slightly defeats the purpose of dce, i'll fix it later™ *)
      | TAC_call _
      | TAC_dispatch _
      | TAC_inline_assembly _
      | TAC_void_check _
      | TAC_internal _
      | TAC_div _ (* I don't like this, but it's the simplest way to ensure div by 0 errors don't get eliminated *)
      | TAC_attribute _ -> true
      | _ -> false
    in
  
    let changed = ref true in
    while !changed do
      changed := false;
      Hashtbl.iter (fun id block ->
        let info = Hashtbl.find live_info id in
  
        (* Compute live_out = union of successors' live_in *)
        let new_live_out =
          List.fold_left (fun acc succ_id ->
            let succ_info = Hashtbl.find live_info succ_id in
            IdSet.union acc succ_info.live_in
          ) IdSet.empty block.successors
        in
  
        let use, def =
          List.fold_left (fun (use, def) cmd ->
            let u, d = uses_and_defs cmd in
            (IdSet.union use (IdSet.diff u def), IdSet.union def d)
          ) (IdSet.empty, IdSet.empty) block.instructions
        in
  
        (* live_in = use union (live_out - def) *)
        let new_live_in = IdSet.union use (IdSet.diff new_live_out def) in
  
        if not (IdSet.equal new_live_in info.live_in) || not (IdSet.equal new_live_out info.live_out) then begin
          info.live_in <- new_live_in;
          info.live_out <- new_live_out;
          changed := true;
        end
      ) mthd.blocks;
    done;
  
    Hashtbl.iter (fun id block ->
      let info = Hashtbl.find live_info id in
      let live = ref info.live_out in
      let new_instrs =
        List.fold_right (fun cmd acc ->
          let uses, defs = uses_and_defs cmd in
          let side_uses = side_effecting_uses cmd in
          if not (IdSet.is_empty defs) && IdSet.is_empty (IdSet.inter defs !live) && not (is_critical cmd) then
            acc
          else begin
            live := IdSet.union !live side_uses;
            live := IdSet.union (IdSet.diff !live defs) uses;
            cmd :: acc
          end
        ) block.instructions []
      in
      Hashtbl.replace mthd.blocks id { block with instructions = new_instrs }
    ) mthd.blocks;
  
    mthd
  
    
let eliminate_dead_code (graph : cfg) : cfg = 
  List.map dce_method graph

(* please future Tommy add comments before merging *)
let rec get_all_used_methods (mthd : ast_method) (cls : ast_class) (ast : ast) (acc : class_and_method list): class_and_method list = 
  let object_methods = ["abort"; "copy"; "type_name"] in
  let io_methods = ["in_string"; "out_string"; "in_int"; "out_int"] in
  let string_methods = ["length"; "concat"; "substr"] in
  let already_visited c m = List.mem {class_name = c; method_name = m} acc in
  let rec inherits_io class_name = 
    if List.mem class_name ["Bool"; "Object"; "Int"; "String";] then false 
      else if class_name = "IO" then true else (
    Printf.printf "Inherits IO - class %s\n" class_name;
    match (List.find (fun (m : ast_class) -> (m.name.name = class_name)) ast).inherits with
    | None -> false
    | Some inherited -> if inherited.name = "IO" then true else inherits_io inherited.name
    )
    in

  let rec extract_used (value : ast_expression_val) (cls : ast_class) (ast : ast): class_and_method list =
    let rec extract_expr_list stmts = 
      match stmts with | [] -> [] | hd :: tl -> (extract_used hd.data cls ast@ extract_expr_list tl) in

    match value with 
    | Assign expr -> (extract_used expr.rhs.data cls ast)
    | If expr -> (
      (extract_used expr.predicate.data cls ast) @ 
      (extract_used expr._then.data cls ast) @ (extract_used expr._else.data cls ast)
    )
    | While expr -> (
      (extract_used expr.predicate.data cls ast) @ (extract_used expr.body.data cls ast)
    )
    | Block expr_list -> (
        extract_expr_list expr_list.body
    )
    | New expr -> (
      [{class_name = expr._class.name; method_name = String.empty}]
    )
    (* why wouldn't this combine??? *)
    | UnOp expr -> (extract_used expr.expr.data cls ast)
    | IsVoid expr -> (extract_used expr.expr.data cls ast)
    | BinOp expr -> (
      (* maybe signal comparison code is needed *)
      (extract_used expr.left.data cls ast) @ (extract_used expr.right.data cls ast)
    )
    | Let expr -> (
      let rec extract_let_bindings (bd : ast_let_binding_type list) : class_and_method list = 
        match bd with 
        | [] -> []
        | hd :: tl -> (
          match hd with
          LetBindingNoInit no_init -> (
            {class_name = no_init._type.name; method_name = String.empty} :: extract_let_bindings tl
          )
        | LetBindingInit init -> (
          {class_name = init._type.name; method_name = String.empty} 
            :: (extract_used init.value.data cls ast) @ extract_let_bindings tl
        )
        )
      in
      (extract_let_bindings expr.bindings) @ (extract_used expr._in.data cls ast)
    )
    | Case expr -> (
      let rec extract_case_mappings (cs : ast_case_mapping list) : class_and_method list = 
        match cs with 
        | [] -> []
        | hd :: tl -> (
          {class_name = hd._type.name; method_name = String.empty} 
            :: extract_used hd.maps_to.data cls ast @ extract_case_mappings tl
        )
      in
      (extract_case_mappings expr.mapping_list) @ (extract_used expr.expression.data cls ast)
    )

    | DynamicDispatch expr -> (
      if not ((List.mem expr._method.name (object_methods @ io_methods) && inherits_io expr.call_on._type) 
        || (List.mem expr._method.name (object_methods @ string_methods) && expr.call_on._type = "String")
        || (List.mem expr._method.name object_methods) || already_visited expr.call_on._type expr._method.name)
      then (
      Printf.printf "DD - class %s, method %s \n" expr.call_on._type expr._method.name;
      let new_class = List.find (fun (m : ast_class) -> (m.name.name = expr.call_on._type)) ast in
      let new_method = List.find (fun (m : ast_method) -> (m.name.name = expr._method.name)) new_class.methods in
      let new_acc = {class_name = expr.call_on._type; method_name = expr._method.name} :: extract_used expr.call_on.data cls ast
       @ (extract_expr_list expr.args) in
        get_all_used_methods new_method new_class ast new_acc
       )
      else {class_name = expr.call_on._type; method_name = expr._method.name} :: extract_used expr.call_on.data cls ast
      @ (extract_expr_list expr.args)
    )
    | StaticDispatch expr -> (
      if not ((List.mem expr._method.name (object_methods @ io_methods) && inherits_io expr._type.name) 
        || (List.mem expr._method.name (object_methods @ string_methods) && expr._type.name = "String")
        || (List.mem expr._method.name object_methods) || already_visited expr._type.name expr._method.name)
      then (
        Printf.printf "SD - class %s, method %s \n" expr._type.name expr._method.name;
        let new_class = List.find (fun (m : ast_class) -> (m.name.name = expr._type.name)) ast in
        let new_method = List.find (fun (m : ast_method) -> (m.name.name = expr._method.name)) new_class.methods in
        let new_acc = {class_name = expr._type.name; method_name = expr._method.name}
         :: extract_used expr.call_on.data cls ast @ (extract_expr_list expr.args) in
        get_all_used_methods new_method new_class ast new_acc
      )
      else {class_name = expr._type.name; method_name = expr._method.name} :: extract_used expr.call_on.data cls ast
        @ (extract_expr_list expr.args)
    )
    | SelfDispatch expr -> (
      if not ((List.mem expr._method.name (object_methods @ io_methods) && inherits_io cls.name.name) 
        || (List.mem expr._method.name (object_methods @ string_methods) && cls.name.name = "String")
        || (List.mem expr._method.name object_methods) || already_visited cls.name.name expr._method.name)
      then (
        Printf.printf "Self - class %s, method %s \n" cls.name.name expr._method.name;
        let new_class = List.find (fun (m : ast_class) -> (m.name.name = cls.name.name)) ast in
        let new_method = List.find (fun (m : ast_method) -> (m.name.name = expr._method.name)) new_class.methods in
        let new_acc = {class_name = cls.name.name; method_name = expr._method.name} :: (extract_expr_list expr.args) in
        get_all_used_methods new_method new_class ast new_acc
      )
      else {class_name = cls.name.name; method_name = expr._method.name} :: (extract_expr_list expr.args)
    )
    (* Maybe bool/int/string will have to be matched and added *)
    | _ -> []
  in
  acc @ (extract_used mthd.body.data cls ast)

let print_used_methods cm = Printf.printf "Class: %s --- Method: %s \n" cm.class_name cm.method_name; ()
let rec remove_dupes lst = match lst with
   | [] -> []
   | hd :: tl -> if List.mem hd tl then remove_dupes tl else hd :: remove_dupes tl

let remove_unused_methods (data : program_data) : program_data = 
  print_endline "Finding main";
  let main_class = List.find (fun (m : ast_class) -> (m.name.name = "Main")) data.ast in
  let main_method = List.find (fun (m : ast_method) -> (m.name.name = "main")) main_class.methods in
  (* this could/should be a set but it shouldn't really matter *)
  let all_methods = remove_dupes (get_all_used_methods main_method main_class data.ast [])  in
  Printf.printf "Used %d class/method combinations \n" (List.length all_methods);
  List.iter print_used_methods all_methods;
  data