open B_ast
open B_class_map
open B_impl_map
open B_parent_map
open C_parser_data
open D_tac_data

module StringTbl = Hashtbl.Make (struct
    type t = string
    let equal = String.equal
    let hash = Hashtbl.hash
end)

type symbol_table = tac_id StringTbl.t

let tac_gen_expr_body (data : parsed_data) (_class : ast_class) (method_body : ast_expression) (symbol_table : symbol_table ref) : (tac_id list * tac_cmd list) =
    let tac_counter : int ref = ref 0 in
    let tac_id_list : tac_id list ref = ref [] in

    let add_symbol (x : string) (id : tac_id) : unit =
        StringTbl.add !symbol_table x id;
        tac_id_list := id :: !tac_id_list;
    in

    let remove_symbol (x : string) : unit =
        StringTbl.remove !symbol_table x
    in

    let find_symbol (x : string) : tac_id =
        match StringTbl.find_opt !symbol_table x with
        | Some id -> id
        | None -> raise (Invalid_argument ("Symbol not found: " ^ x))
    in

    let get_running_id () : tac_id =
        let id = !tac_counter in
        let id_as_str = Printf.sprintf "t$%d" id in

        tac_id_list := id_as_str :: !tac_id_list;
        
        tac_counter := id + 1;
        id_as_str
    in

    let decline_id () : unit =
        tac_counter := !tac_counter - 1;
        tac_id_list := List.tl !tac_id_list
    in

    let rec escape_backslashes s =
        let len = String.length s in
        let rec aux i acc =
          if i >= len then String.concat "" (List.rev acc)
          else if s.[i] = '\\' && i + 1 < len then
            let next_char = s.[i + 1] in
            if next_char <> 't' && next_char <> 'n' then
              aux (i + 2) (("\\" ^ "\\" ^ String.make 1 next_char) :: acc)
            else
              aux (i + 2) (("\\" ^ String.make 1 next_char) :: acc)
          else
            aux (i + 1) (String.make 1 s.[i] :: acc)
        in
        aux 0 []
    
    in

    let rec rec_tac_gen (expr : ast_expression) : (tac_id * tac_cmd list) =
        let self_id = get_running_id () in
    
        match expr.data with
        | Assign            { var; rhs } ->
            let var_id = find_symbol var.name in
            let (rhs_id, rhs_cmds) = rec_tac_gen rhs in

            let assign_cmd = TAC_ident (var_id, rhs_id) in

            (var_id, rhs_cmds @ [assign_cmd])
        | DynamicDispatch   { call_on; _method; args } ->
            let (obj_id, obj_cmds) = rec_tac_gen call_on in
            let (args_ids, args_cmds) = List.split (List.map rec_tac_gen args) in

            let dispatch = get_dispatch data call_on._type _method.name in

            let call_cmd = TAC_call (self_id, dispatch, args_ids) in
            (self_id, obj_cmds @ List.concat args_cmds @ [call_cmd])
        | StaticDispatch    { call_on; _type; _method; args; } ->
            let (obj_id, obj_cmds) = rec_tac_gen call_on in
            let (args_ids, args_cmds) = List.split (List.map rec_tac_gen args) in

            let dispatch = get_dispatch data _type.name _method.name in

            let call_cmd = TAC_call (self_id, dispatch, args_ids) in
            (self_id, obj_cmds @ List.concat args_cmds @ [call_cmd])
        | SelfDispatch      { _method; args } ->
            let (args_ids, args_cmds) = List.split (List.map rec_tac_gen args) in

            let dispatch = get_dispatch data (_class.name.name) _method.name in

            let call_cmd = TAC_call (self_id, dispatch, args_ids) in
            (self_id, List.concat args_cmds @ [call_cmd])
        | If                { predicate; _then; _else } ->
            let (cond_id, cond_cmds) = rec_tac_gen predicate in
            let (then_id, then_cmds) = rec_tac_gen _then in
            let (else_id, else_cmds) = rec_tac_gen _else in

            let if_id = get_running_id () in

            let then_name = if_id ^ "_then" in
            let else_name = if_id ^ "_else" in
            let merge_name = if_id ^ "_merge" in

            let bt_cmd = TAC_bt (cond_id, then_name) in
            let jmp_cmd = TAC_jmp else_name in

            let label_then  = TAC_label then_name in
            let label_else  = TAC_label else_name in
            let label_merge = TAC_label merge_name in

            let condition = cond_cmds @ [bt_cmd; jmp_cmd] in
            let then_ = [label_then] @ then_cmds @ [TAC_ident (self_id, then_id)] @ [TAC_jmp merge_name] in
            let else_ = [label_else] @ else_cmds @ [TAC_ident (self_id, else_id)] @ [label_merge] in

            (self_id, condition @ then_ @ else_)
        | While             { predicate; body } ->
            let (cond_id, cond_cmds) = rec_tac_gen predicate in
            let (body_id, body_cmds) = rec_tac_gen body in

            let while_id = get_running_id () in

            let cond_name = while_id ^ "_cond" in
            let body_name = while_id ^ "_body" in
            let merge_name = while_id ^ "_merge" in

            let bt_cmd = TAC_bt (cond_id, body_name) in
            let jmp_cmd = TAC_jmp merge_name in

            let label_body = TAC_label body_name in
            let label_cond = TAC_label cond_name in
            let label_merge = TAC_label merge_name in

            let condition = [label_cond] @ cond_cmds @ [bt_cmd; jmp_cmd] in
            let body_ = label_body :: body_cmds @ [TAC_jmp cond_name] in

            (self_id, condition @ body_ @ [label_merge])
        | Block            { body } ->
            decline_id ();
            let (ids, cmds) = List.split (List.map rec_tac_gen body) in

            let rec last_id (ids : tac_id list) : tac_id =
                match ids with
                | [] -> self_id
                | [id] -> id
                | _ :: tl -> last_id tl
            in

            (last_id ids, List.concat cmds)
        | New              { _class } ->
            (self_id, [TAC_new (self_id, _class.name)])
        | IsVoid           { expr } ->
            let (expr_id, expr_cmds) = rec_tac_gen expr in

            (self_id, expr_cmds @ [TAC_isvoid (self_id, expr_id)])
        | BinOp             { left; right; op } ->
            let (lhs_id, lhs_cmds) = rec_tac_gen left in
            let (rhs_id, rhs_cmds) = rec_tac_gen right in

            let cmd = match op with
                | Plus      -> TAC_add (self_id, lhs_id, rhs_id)
                | Minus     -> TAC_sub (self_id, lhs_id, rhs_id)
                | Times     -> TAC_mul (self_id, lhs_id, rhs_id)
                | Divide    -> TAC_div (self_id, lhs_id, rhs_id)

                | LT        -> TAC_lt  (self_id, lhs_id, rhs_id)
                | LE        -> TAC_lte (self_id, lhs_id, rhs_id)
                | EQ        -> TAC_eq  (self_id, lhs_id, rhs_id)
            in
            
            (self_id, lhs_cmds @ rhs_cmds @ [cmd])
        | UnOp             { expr; op } ->
            let (expr_id, expr_cmds) = rec_tac_gen expr in

            let cmd = match op with
                | Not       -> TAC_not (self_id, expr_id)
                | Negate    -> TAC_neg (self_id, expr_id)
            in

            (self_id, expr_cmds @ [cmd])
        | Integer           i -> 
            (self_id, [TAC_int (self_id, i)])
        | String            s ->
            let escaped_s = escape_backslashes s in
            (self_id, [TAC_str (self_id, escaped_s)])
        | True                -> 
            (self_id, [TAC_bool (self_id, true)])
        | False               ->
            (self_id, [TAC_bool (self_id, false)])
        | Identifier        ident ->
            let var_name = find_symbol ident.name in

            (self_id, [TAC_ident (self_id, var_name)])
        | Let               { bindings; _in } ->
            decline_id ();

            let tac_initialize (binding : ast_let_binding_type) : tac_cmd list =
                
                match binding with
                | LetBindingNoInit  { variable; _type } ->
                    let id = get_running_id () in
                    add_symbol variable.name id;
                    [TAC_default (id, _type.name)]
                | LetBindingInit    { variable; _type; value } ->
                    let (rhs_id, rhs_cmds) = rec_tac_gen value in
                    add_symbol variable.name rhs_id;
                    rhs_cmds
            in

            let tac_remove_binding (binding : ast_let_binding_type) : unit =
                match binding with
                | LetBindingNoInit  { variable; _type } ->
                    remove_symbol variable.name
                | LetBindingInit    { variable; _type; value } ->
                    remove_symbol variable.name
            in

            let init_cmds = List.concat (List.map tac_initialize bindings) in
            let (in_id, in_cmds) = rec_tac_gen _in in

            List.iter (tac_remove_binding) bindings;

            (in_id, init_cmds @ in_cmds)
        | Case              { expression; mapping_list } ->
            (self_id, [TAC_comment "Case not implemented"])
        | Internal         _ -> 
            (self_id, [TAC_comment "Internal expression not implemented"])
    in

    let (tac_id, tac_cmds) = rec_tac_gen method_body in
    (!tac_id_list, tac_cmds @ [TAC_return tac_id])