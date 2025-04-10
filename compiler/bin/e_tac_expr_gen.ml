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
    (temp_counter : int ref) (local_counter : int ref) : tac_id * (tac_id list * tac_cmd list) =

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

    let cast_val (value : tac_id) (val_type : string) (cast_type : string) : tac_id * tac_cmd list =
        let val_type = if val_type = "SELF_TYPE" then class_name else val_type in
        let cast_type = if cast_type = "SELF_TYPE" then class_name else cast_type in

        (* Printf.printf "Casting %s (%s) to %s\n" (f_id value) val_type cast_type; *)

        match val_type, cast_type with
        | "Object", "Int"
        | "Object", "String"
        | "Object", "Bool" ->
            let temp = temp_id () in
            temp, [TAC_call (temp, "lift_val", [value])]
        | "Int", "Object" ->
            let temp = temp_id () in
            temp, [TAC_call (temp, "unlift_int", [value])]
        | "String", "Object" ->
            let temp = temp_id () in
            temp, [TAC_call (temp, "unlift_string", [value])]
        | "Bool", "Object" ->
            let temp = temp_id () in
            temp, [TAC_call (temp, "unlift_bool", [value])]
        | _ -> value, []
    in

    let rec escape_backslashes s =
        let len = String.length s in
        let rec aux i acc =
          if i >= len then String.concat "" (List.rev acc)
          else if s.[i] = '"' then
            aux (i + 1) ("\\\"" :: acc)
          else if s.[i] = '\\' then
            aux (i + 1) ("\\\\" :: acc)
          else
            aux (i + 1) ((String.make 1 s.[i]) :: acc)
        in
        aux 0 []
    
    in

    let rec rec_tac_gen (expr : ast_expression) : (tac_id * tac_cmd list) =
        let gen_args (_types : string list) (args : ast_expression list) : (tac_id list * tac_cmd list list) =
            if (List.length _types) <> (List.length args) then
                let text = Printf.sprintf "Expected: %d args, Got: %d" (List.length _types) (List.length args) in
                Printf.printf "Error: %s\n" text;
                exit 1;
            else
            
            List.split @@ List.map(
                fun (arg, _type) ->
                    let arg_id, arg_cmds = rec_tac_gen arg in
                    let casted_id, casted_cmds = cast_val arg_id arg._type _type in

                    casted_id, arg_cmds @ casted_cmds
            ) @@ List.combine args _types
        in

        match expr.data with
        | Assign            { var; rhs } ->
            let var_id = find_symbol var.name in
            let (rhs_id, rhs_cmds) = rec_tac_gen rhs in

            let var_type = find_symbol_type var.name in
            let casted_id, casted_cmds = cast_val rhs_id rhs._type var_type in

            (casted_id, rhs_cmds @ casted_cmds @ [TAC_ident (var_id, casted_id)])
        | DynamicDispatch   { call_on; _method; args } ->
            let call_on_type = if call_on._type = "SELF_TYPE" then class_name else call_on._type in

            let return_type, arg_types = get_method_signature data call_on_type _method.name in

            let (obj_id, obj_cmds) = rec_tac_gen call_on in
            let (args_ids, args_cmds) = gen_args arg_types args in

            let self_id = temp_id () in

            let comment = TAC_comment ("DynamicDispatch: " ^ _method.name) in
            let check_dispatch = [TAC_void_check (_method.line_number, obj_id, "error_dispatch")] in

            if not (StringSet.mem call_on._type data.overriden_classes) then
                let dispatch = method_name_gen call_on_type _method.name in

                if _method.name = "copy" && (
                    call_on._type = "Int" ||
                    call_on._type = "Bool" ||
                    call_on._type = "String"
                ) then
                    (self_id, obj_cmds @ List.concat args_cmds @ [comment; TAC_ident (self_id, obj_id)])
                else
                    let call_cmd = TAC_call (self_id, dispatch, obj_id :: args_ids) in
                    let casted_id, casted_cmds = cast_val self_id return_type call_on._type in
                    (casted_id, obj_cmds @ check_dispatch @ List.concat args_cmds @ [comment; call_cmd] @ casted_cmds)
            else

            let method_id = get_dispatch data call_on_type _method.name in

            let call_cmd = TAC_dispatch { 
                line_number = _method.line_number;
                store = self_id; 
                obj = obj_id;
                method_id;
                args = obj_id :: args_ids;
            } in

            let casted_id, casted_cmds = cast_val self_id return_type call_on._type in

            (self_id, obj_cmds @ check_dispatch @ (List.concat args_cmds @ [comment; call_cmd] @ casted_cmds))
        | StaticDispatch    { call_on; _type; _method; args; } ->
            let return_type, arg_types = get_method_signature data _type.name _method.name in

            let (obj_id, obj_cmds) = rec_tac_gen call_on in
            let (args_ids, args_cmds) = gen_args arg_types args in
            
            let self_id = temp_id () in
            let check_dispatch = [TAC_void_check (_method.line_number, obj_id, "error_dispatch")] in

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
            let casted_id, casted_cmds = cast_val self_id return_type _type.name in

            (casted_id, obj_cmds @ check_dispatch @ List.concat args_cmds @ [comment; call_cmd] @ casted_cmds)
        | SelfDispatch      { _method; args } ->
            let return_type, arg_types = get_method_signature data class_name _method.name in

            let (args_ids, args_cmds) = gen_args arg_types args in
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

            let casted_id, casted_cmds = cast_val self_id return_type class_name in
            
            (casted_id, List.concat args_cmds @ [comment; call_cmd] @ casted_cmds)
        | If                { predicate; _then; _else } ->
            let (cond_id, cond_cmds) = rec_tac_gen predicate in
            let (then_id, then_cmds) = rec_tac_gen _then in
            let (else_id, else_cmds) = rec_tac_gen _else in

            (* This isn't strictly correct, but to the compiler, all that matters is whether a
               type is a lifted int/string/bool or an object. *)
            let merge_type = match _then._type, _else._type with
                | "Int", "Int" -> "Int"
                | "String", "String" -> "String"
                | "Bool", "Bool" -> "Bool"
                | _ -> "Object"
            in

            let casted_then_id, casted_then_cmds = cast_val then_id _then._type merge_type in
            let casted_else_id, casted_else_cmds = cast_val else_id _else._type merge_type in
            
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
            let then_ = [label_then] @ then_cmds @ casted_then_cmds @ [TAC_ident (self_id, casted_then_id)] @ [TAC_jmp merge_name] in
            let else_ = [label_else] @ else_cmds @ casted_else_cmds @ [TAC_ident (self_id, casted_else_id)] @ [label_merge] in

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

            let condition = label_cond :: cond_cmds @ [bt_cmd; jmp_cmd] in
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

                | LT        ->
                    begin match left._type with
                    | "String" -> TAC_str_lt (self_id, lhs_id, rhs_id)
                    | _        -> TAC_lt (self_id, lhs_id, rhs_id)
                    end
                | LE        ->
                    begin match left._type with
                    | "String" -> TAC_str_lte (self_id, lhs_id, rhs_id)
                    | _        -> TAC_lte (self_id, lhs_id, rhs_id)
                    end
                | EQ        -> 
                    begin match left._type with
                    | "String" -> TAC_str_eq (self_id, lhs_id, rhs_id)
                    | _        -> TAC_eq (self_id, lhs_id, rhs_id)
                    end
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
                    let (casted_id, casted_cmds) = cast_val rhs_id value._type _type.name in
                    add_symbol variable.name casted_id _type.name;

                    TAC_default (casted_id, _type.name) :: rhs_cmds @ casted_cmds
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
            let merge_type = expr._type in

            let merge_val = temp_id () in
            let type_name = temp_id () in

            let type_cmd =
                match expression._type with
                | "Int" | "String" | "Bool" -> 
                    let method_name = method_name_gen (expression._type) "type_name" in
                    
                    [ TAC_call (type_name, method_name, []) ]
                | _ ->
                    [
                        TAC_void_check (expression.ident.line_number, expr_id, "error_case_void");
                        TAC_dispatch { 
                            line_number = expression.ident.line_number;
                            store = type_name; 
                            obj = expr_id;
                            method_id = 2; (* type_name *)
                            args = [expr_id];
                        }
                    ]
            in

            let cmds = expr_cmds @ type_cmd in
            let merge_label = label_id () ^ "_case_merge" in

            let generate_case_commands (mapping : ast_case_mapping) =
                let label = label_id () ^ "_case" in
                let casted_id, casted_cmds = cast_val expr_id expression._type mapping._type.name in

                add_symbol mapping.name.name casted_id mapping._type.name;
                let body_id, body_cmds = rec_tac_gen mapping.maps_to in
                remove_symbol mapping.name.name;

                let casted_body_id, cast_body_cmds = cast_val body_id mapping.maps_to._type merge_type in

                let cond = temp_id () in
                let str = temp_id () in

                let trace = [mapping._type.name] @ get_subtypes data mapping._type.name in

                let jump = match mapping._type.name with
                    | "Object" -> [TAC_jmp label]
                    | name ->
                        trace
                        |> List.map (
                            fun _type ->
                                [
                                    TAC_str (str, _type);
                                    TAC_str_eq (cond, type_name, str);
                                    TAC_bt (cond, label)
                                ]
                            ) 
                        |> List.concat
                in

                jump, TAC_label label :: casted_cmds @ body_cmds @ cast_body_cmds @ [
                    TAC_ident (merge_val, casted_body_id);
                    TAC_jmp merge_label
                ]
            in

            let jumps, bodies = 
                mapping_list
                |> List.sort (fun (m1 : ast_case_mapping) (m2 : ast_case_mapping) -> 
                    compare
                        (type_order data m1._type.name)
                        (type_order data m2._type.name)
                    )
                |> List.rev
                |> List.map (generate_case_commands)
                |> List.split
            in

            let match_fail = [
                TAC_inline_assembly ("movq\t  $" ^ string_of_int expr.ident.line_number ^ ", %rsi");
                TAC_jmp "error_case_unmatched";
            ] in

            (merge_val, cmds @ List.concat jumps @ match_fail @ List.concat bodies @ [TAC_label merge_label])
        | Internal         _ -> 
            (temp_id (), [TAC_comment "Internal expression not implemented"])
    in

    let (tac_id, tac_cmds) = rec_tac_gen method_body in
    let (casted_id, casted_cmds) = cast_val tac_id method_body._type return_type in
    
    casted_id, (!tac_id_list, tac_cmds @ casted_cmds)