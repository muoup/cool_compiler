open A_util
open B_ast
open B_class_map
open B_impl_map
open B_parent_map
open C_parser_data
open D_tac_data

let label_id : int ref = ref 0

let label_id () : string =
    let id = !label_id in
    label_id := !label_id + 1;
    Printf.sprintf "L%d" id

let rec last_id (ids : tac_id list) : tac_id =
    match ids with
    | [] -> raise (Invalid_argument "Empty list")
    | [id] -> id
    | _ :: tl -> last_id tl

let tac_gen_expr_body 
    (data : program_data) (class_name : string) (return_type : string) 
    (method_body : ast_expression) (symbol_table : symbol_table ref) 
    (temp_counter : int ref) (local_counter : int ref) : (tac_id list * tac_cmd list) =
    
    let tac_id_list : tac_id list ref = ref [] in

    let add_symbol (x : string) (id : tac_id) (_type : string) : unit =
        StringTbl.add !symbol_table x (id, _type);
        tac_id_list := id :: !tac_id_list;
    in

    let remove_symbol (x : string) : unit =
        StringTbl.remove !symbol_table x
    in

    let find_symbol (x : string) : tac_id =
        match StringTbl.find_opt !symbol_table x with
        | Some (id, _) -> id
        | None -> raise (Invalid_argument ("Symbol not found: " ^ x))
    in

    let find_symbol_type (x : string) : string =
        match StringTbl.find_opt !symbol_table x with
        | Some (_, _type) -> _type
        | None -> raise (Invalid_argument ("Symbol not found: " ^ x))
    in  

    let local_id () : tac_id =
        let id = Local !local_counter in
        local_counter := !local_counter + 1;
        tac_id_list := id :: !tac_id_list;
        id
    in

    let temp_id () : tac_id =
        let id = Temporary !temp_counter in
        temp_counter := !temp_counter + 1;
        tac_id_list := id :: !tac_id_list;
        id
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

    let unlift_val (var_name : string) (rhs_id : tac_id) (rhs_cmds : tac_cmd list) (unlift_procedure : string) : tac_cmd list =
        let self_id = temp_id () in
        add_symbol var_name self_id "Object";
        rhs_cmds @ [ TAC_call (self_id, unlift_procedure, [rhs_id] )]
    in

    let rec rec_tac_gen (expr : ast_expression) : (tac_id * tac_cmd list) =
        let gen_args (args : ast_expression list) : (tac_id list * tac_cmd list list) =
            let (args_ids, args_cmds) = List.split (List.map rec_tac_gen args) in
            let args_ids = args_ids in

            args_ids, args_cmds
        in

        match expr.data with
        | Assign            { var; rhs } ->
            let var_id = find_symbol var.name in
            let (rhs_id, rhs_cmds) = rec_tac_gen rhs in

            let unlift = unlift_val var.name rhs_id rhs_cmds in
            let var_type = find_symbol_type var.name in

            let cmds = begin match var_type, rhs._type with
            | "Object", "Int" -> unlift "unlift_int"
            | "Object", "String" -> unlift "unlift_string"
            | "Object", "Bool" -> unlift "unlift_bool"

            | _ ->
                let assign_cmd = TAC_ident (var_id, rhs_id) in
                rhs_cmds @ [assign_cmd]
            end in

            (var_id, cmds)
        | DynamicDispatch   { call_on; _method; args } ->
            let (obj_id, obj_cmds) = rec_tac_gen call_on in
            let (args_ids, args_cmds) = gen_args args in

            let self_id = temp_id () in

            let comment = TAC_comment ("DynamicDispatch: " ^ _method.name) in

            let check_dispatch = [TAC_void_check (_method.line_number, obj_id)] in

            if not (StringSet.mem call_on._type data.overriden_classes) then
                let dispatch = method_name_gen call_on._type _method.name in

                if _method.name = "copy" && (
                    call_on._type = "Int" ||
                    call_on._type = "Bool" ||
                    call_on._type = "String"
                ) then
                    (self_id, obj_cmds @ List.concat args_cmds @ [comment; TAC_ident (self_id, obj_id)])
                else
                    let call_cmd = TAC_call (self_id, dispatch, obj_id :: args_ids) in
                    (self_id, obj_cmds @ check_dispatch @ List.concat args_cmds @ [comment; call_cmd])
            else

            let method_id = get_dispatch data call_on._type _method.name in

            let call_cmd = TAC_dispatch { 
                line_number = _method.line_number;
                store = self_id; 
                obj = obj_id;
                method_id;
                args = obj_id :: args_ids;
            } in
            (self_id, obj_cmds @ check_dispatch @ (List.concat args_cmds @ [comment; call_cmd]))
        | StaticDispatch    { call_on; _type; _method; args; } ->
            let (obj_id, obj_cmds) = rec_tac_gen call_on in
            let (args_ids, args_cmds) = gen_args args in
            
            let self_id = temp_id () in
            let check_dispatch = [TAC_void_check (_method.line_number, obj_id)] in

            if _method.name = "copy" && (
                _type.name = "Int" ||
                _type.name = "Bool" ||
                _type.name = "String"
            ) then
                (self_id, obj_cmds @ List.concat args_cmds @ [TAC_ident (self_id, obj_id)])
            else

            let comment = TAC_comment ("StaticDispatch: " ^ _type.name ^ "." ^ _method.name) in
            let dispatch = method_name_gen _type.name _method.name in

            let call_cmd = TAC_call (self_id, dispatch, obj_id :: args_ids) in
            (self_id, obj_cmds @ check_dispatch @ List.concat args_cmds @ [comment; call_cmd])
        | SelfDispatch      { _method; args } ->
            let (args_ids, args_cmds) = gen_args args in
            let self_id = temp_id () in
            let comment = TAC_comment ("SelfDispatch: " ^ _method.name) in

            if not (StringSet.mem class_name data.overriden_classes) then
                let dispatch = method_name_gen class_name _method.name in

                let call_cmd = TAC_call (self_id, dispatch, Self :: args_ids) in
                (self_id, List.concat args_cmds @ [comment; call_cmd])
            else

            let dispatch = get_dispatch data class_name _method.name in

            let call_cmd = TAC_dispatch {
                line_number = _method.line_number;
                store = self_id; 
                obj = Self;
                method_id = dispatch;
                args = Self :: args_ids;
            } in
            
            (self_id, List.concat args_cmds @ [comment; call_cmd])
        | If                { predicate; _then; _else } ->
            let (cond_id, cond_cmds) = rec_tac_gen predicate in
            let (then_id, then_cmds) = rec_tac_gen _then in
            let (else_id, else_cmds) = rec_tac_gen _else in

            let self_id = temp_id () in

            let then_name = label_id () ^ "_then" in
            let else_name = label_id () ^ "_else" in
            let merge_name = label_id () ^ "_merge" in

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

            let self_id = temp_id () in

            let cond_name = label_id () ^ "_cond" in
            let body_name = label_id () ^ "_body" in
            let merge_name = label_id () ^ "_merge" in

            let bt_cmd = TAC_bt (cond_id, body_name) in
            let jmp_cmd = TAC_jmp merge_name in

            let label_body = TAC_label body_name in
            let label_cond = TAC_label cond_name in
            let label_merge = TAC_label merge_name in

            let condition = [label_cond] @ cond_cmds @ [bt_cmd; jmp_cmd] in
            let body_ = label_body :: body_cmds @ [TAC_jmp cond_name] in

            (self_id, condition @ body_ @ [label_merge])
        | Block            { body } ->
            let (ids, cmds) = List.split (List.map rec_tac_gen body) in

            (last_id ids, List.concat cmds)
        | New              { _class } ->
            let self_id = temp_id () in

            let _type = if _class.name = "SELF_TYPE" then class_name else _class.name in

            begin match _type with
            | "Int" | "Bool" | "String" ->
                (self_id, [TAC_default (self_id, _type)])
            | _ ->
                (self_id, [TAC_new (self_id, _type)])
            end
        | IsVoid           { expr } ->
            let (expr_id, expr_cmds) = rec_tac_gen expr in
            let self_id = temp_id () in

            begin match expr._type with
            (* isvoid on intrinsic types is always false *)
            | "String" | "Int" | "Bool" -> (self_id, expr_cmds @ [TAC_bool (self_id, false)])
            | _ -> (self_id, expr_cmds @ [TAC_isvoid (self_id, expr_id)])
            end
        | BinOp             { left; right; op } ->
            let (lhs_id, lhs_cmds) = rec_tac_gen left in
            let (rhs_id, rhs_cmds) = rec_tac_gen right in
            let self_id = temp_id () in

            let cmd = match op with
                | Plus      -> TAC_add (self_id, lhs_id, rhs_id)
                | Minus     -> TAC_sub (self_id, lhs_id, rhs_id)
                | Times     -> TAC_mul (self_id, lhs_id, rhs_id)
                | Divide    -> TAC_div (left.ident.line_number, self_id, lhs_id, rhs_id)

                | LT        -> TAC_lt  (self_id, lhs_id, rhs_id)
                | LE        -> TAC_lte (self_id, lhs_id, rhs_id)
                | EQ        -> 
                    match left._type with
                    | "String" -> TAC_str_eq (self_id, lhs_id, rhs_id)
                    | _        -> TAC_eq (self_id, lhs_id, rhs_id)
            in
            
            (self_id, lhs_cmds @ rhs_cmds @ [cmd])
        | UnOp             { expr; op } ->
            let (expr_id, expr_cmds) = rec_tac_gen expr in
            let self_id = temp_id () in

            let cmd = match op with
                | Not       -> TAC_not (self_id, expr_id)
                | Negate    -> TAC_neg (self_id, expr_id)
            in

            (self_id, expr_cmds @ [cmd])
        | Integer           i ->
            let self_id = temp_id () in 
            (self_id, [TAC_int (self_id, i)])
        | String            s ->
            let escaped_s = escape_backslashes s in
            let self_id = temp_id () in
            (self_id, [TAC_str (self_id, escaped_s)])
        | True                -> 
            let self_id = temp_id () in
            (self_id, [TAC_bool (self_id, true)])
        | False               ->
            let self_id = temp_id () in
            (self_id, [TAC_bool (self_id, false)])
        | Identifier        ident ->
            let var_name = find_symbol ident.name in
            let self_id = temp_id () in

            (self_id, [TAC_ident (self_id, var_name)])
        | Let               { bindings; _in } ->
            let tac_initialize (binding : ast_let_binding_type) : tac_cmd list =
                match binding with
                | LetBindingNoInit  { variable; _type } ->
                    let id = local_id () in
                    add_symbol variable.name id _type.name;
                    [TAC_default (id, _type.name)]
                | LetBindingInit    { variable; _type; value } ->
                    let (rhs_id, rhs_cmds) = rec_tac_gen value in

                    let unlift = unlift_val variable.name rhs_id rhs_cmds in

                    begin match (_type.name, value._type) with
                    | "Object", "Int" -> unlift "unlift_int"
                    | "Object", "String" -> unlift "unlift_string"
                    | "Object", "Bool" -> unlift "unlift_bool"
                        
                    | _ ->
                        add_symbol variable.name rhs_id _type.name;
                        rhs_cmds
                    end
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
            let (expr_id, expr_cmds) = rec_tac_gen expression in

            let merge_val = temp_id () in
            let type_name = temp_id () in
            let cmds = expr_cmds @ [
                TAC_void_check (expression.ident.line_number, expr_id);
                TAC_dispatch { 
                    line_number = expression.ident.line_number;
                    store = type_name; 
                    obj = expr_id;
                    method_id = 2; (* type_name *)
                    args = [expr_id];
                }
            ] in

            let merge_label = label_id () ^ "_case_merge" in

            let jumps, bodies = List.split @@ List.map (
                fun (mapping : ast_case_mapping) ->
                    let label = label_id () ^ "_case" in

                    add_symbol mapping.name.name expr_id mapping._type.name;
                    (* TODO: Lift Object -> Int/Bool/String*)
                    let (body_id, body_cmds) = rec_tac_gen mapping.maps_to in
                    remove_symbol mapping.name.name;

                    let cond = temp_id () in
                    let str = temp_id () in

                    let jump = [
                        TAC_str (str, mapping._type.name);
                        TAC_str_eq (cond, type_name, str);
                        TAC_bt (cond, label)
                    ] in

                    jump, TAC_label label :: body_cmds @ [ TAC_ident (merge_val, body_id); TAC_jmp merge_label ]
            ) mapping_list in

            (merge_val, cmds @ List.concat jumps @ List.concat bodies @ [TAC_label merge_label])
        | Internal         _ -> 
            (temp_id (), [TAC_comment "Internal expression not implemented"])
    in

    let unlift_id = temp_id () in

    let (tac_id, tac_cmds) = rec_tac_gen method_body in
    
    let return_cmds = match return_type, method_body._type with
        | "Object", "Int" -> 
            [
                TAC_call (unlift_id, "unlift_int", [tac_id]);
                TAC_return unlift_id
            ]
        | "Object", "String" -> 
            [
                TAC_call (unlift_id, "unlift_string", [tac_id]);
                TAC_return unlift_id
            ]
        | "Object", "Bool" -> 
            [
                TAC_call (unlift_id, "unlift_bool", [tac_id]);
                TAC_return unlift_id
            ]
        | _ -> [TAC_return tac_id]
        in

    (!tac_id_list, tac_cmds @ return_cmds)