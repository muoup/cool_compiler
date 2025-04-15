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
    (local_counter : int ref) (global_temp_counter : int ref) : tac_id * (tac_cmd list) =

    let free_temps : tac_id list ref = ref [] in
    let temp_counter : int ref = ref 0 in

    let add_symbol (x : string) (id : tac_id) (_type : string) : unit =
        StringTbl.add !symbol_table x (id, _type);
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
        id
    in

    let temp_id () : tac_id =
        match !free_temps with
        | x :: xs ->
            free_temps := xs;
            x
        | [] ->
            let id = Temporary !temp_counter in
            temp_counter := !temp_counter + 1;
            id    
    in

    let free_temp (id : tac_id) : unit =
        match id with
        | Temporary _ -> 
            if List.mem id !free_temps then
                failwith @@ "Duplicate free! " ^ (f_id id)
            else
                free_temps := id :: !free_temps
        | _ -> ()
    in

    let cast_val_no_free (value : tac_id) (val_type : string) (cast_type : string) : (tac_id * tac_cmd list) =
        let val_type = if val_type = "SELF_TYPE" then class_name else val_type in
        let cast_type = if cast_type = "SELF_TYPE" then class_name else cast_type in
        let _val = temp_id () in

        match val_type, cast_type with
        | "Int", "Int"
        | "String", "String"
        | "Bool", "Bool" -> 
            _val, [TAC_ident (_val, value)]

        | _, "Int"
        | _, "String"
        | _, "Bool" ->
            _val, [TAC_call (_val, "lift_val", [value])]
        | "Int", _ ->
            _val, [TAC_call (_val, "unlift_int", [value])]
        | "String", _ ->
            _val, [TAC_call (_val, "unlift_string", [value])]
        | "Bool", _ ->
            _val, [TAC_call (_val, "unlift_bool", [value])]
        
        | _ ->
            _val, [TAC_ident (_val, value)]
    in

    let cast_val (value : tac_id) (val_type : string) (cast_type : string) : tac_id * tac_cmd list =
        free_temp value;
        cast_val_no_free value val_type cast_type
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

            List.combine args _types
            |> List.map (
                fun (arg, _type) ->
                    let arg_id, arg_cmds = rec_tac_gen arg in
                    let casted_id, casted_cmds = cast_val arg_id arg._type _type in

                    casted_id, arg_cmds @ casted_cmds
                )
            |> List.split
        in

        match expr.data with
        | Assign            { var; rhs } ->
            let var_id = find_symbol var.name in
            let (rhs_id, rhs_cmds) = rec_tac_gen rhs in

            let var_type = find_symbol_type var.name in
            let casted_id, casted_cmds = cast_val_no_free rhs_id rhs._type var_type in

            free_temp casted_id;

            (rhs_id, rhs_cmds @ casted_cmds @ [TAC_ident (var_id, casted_id)])
        | DynamicDispatch   { call_on; _method; args } ->
            let call_on_type = if call_on._type = "SELF_TYPE" then class_name else call_on._type in

            let return_type, arg_types = get_method_signature data call_on_type _method.name in

            let (args_ids, args_cmds) = gen_args arg_types args in
            let (obj_id, obj_cmds) = rec_tac_gen call_on in

            List.iter (free_temp) args_ids;
            free_temp obj_id;

            let self_id = temp_id () in

            let lifted_type = 
                match call_on._type with
                | "Int" | "String" | "Bool" -> true
                | _ -> false
            in
            
            let comment = TAC_comment ("DynamicDispatch: " ^ _method.name) in
            let check_dispatch = 
                if lifted_type then
                    []
                else
                    [TAC_void_check (expr.ident.line_number, obj_id, "error_dispatch")] 
            in

            if not (StringSet.mem call_on._type data.overriden_classes) then
                let dispatch = method_name_gen call_on_type _method.name in

                let call_cmd =
                    if _method.name = "copy" && lifted_type then
                        TAC_ident (self_id, obj_id)
                    else
                        TAC_call (self_id, dispatch, obj_id :: args_ids)
                in

                (self_id, List.concat args_cmds @ obj_cmds @ check_dispatch @ [comment; call_cmd])
            else

            let method_id = get_dispatch data call_on_type _method.name in

            let call_cmd = TAC_dispatch { 
                line_number = _method.line_number;
                store = self_id; 
                obj = obj_id;
                method_id;
                args = obj_id :: args_ids;
            } in

            (self_id, List.concat args_cmds @ obj_cmds @ check_dispatch @ [comment; call_cmd])
        | StaticDispatch    { call_on; _type; _method; args; } ->
            let return_type, arg_types = get_method_signature data _type.name _method.name in

            let (args_ids, args_cmds) = gen_args arg_types args in
            let (obj_id, obj_cmds) = rec_tac_gen call_on in
            
            List.iter (free_temp) args_ids;
            free_temp obj_id;
            
            let self_id = temp_id () in
            let lifted_type = 
                match call_on._type with
                | "Int" | "String" | "Bool" -> true
                | _ -> false
            in
            let check_dispatch = if lifted_type then
                []
            else
                [TAC_void_check (expr.ident.line_number, obj_id, "error_dispatch")]
            in

            if _method.name = "copy" && lifted_type then
                (self_id, obj_cmds @ List.concat args_cmds @ [TAC_ident (self_id, obj_id)])
            else

            let comment = TAC_comment ("StaticDispatch: " ^ _type.name ^ "." ^ _method.name) in
            let dispatch = method_name_gen _type.name _method.name in

            let call_cmd = TAC_call (self_id, dispatch, obj_id :: args_ids) in
            (self_id, List.concat args_cmds @ obj_cmds @ check_dispatch @ [comment; call_cmd])
        | SelfDispatch      { _method; args } ->
            let return_type, arg_types = get_method_signature data class_name _method.name in

            let (args_ids, args_cmds) = gen_args arg_types args in
            let self_id = temp_id () in

            List.iter (free_temp) args_ids;
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
            (* This isn't strictly correct, but to the compiler, all that matters is whether a
               type is a lifted int/string/bool or an object. *)
            let merge_type = match _then._type, _else._type with
               | "Int", "Int" -> "Int"
               | "String", "String" -> "String"
               | "Bool", "Bool" -> "Bool"
               | _ -> "Object"
            in

            let (cond_id, cond_cmds) = rec_tac_gen predicate in
            free_temp cond_id;

            let (then_id, then_cmds) = rec_tac_gen _then in
            let casted_then_id, casted_then_cmds = cast_val then_id _then._type merge_type in
            free_temp casted_then_id;
            
            let (else_id, else_cmds) = rec_tac_gen _else in
            let casted_else_id, casted_else_cmds = cast_val else_id _else._type merge_type in
            free_temp casted_else_id;
            
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
            free_temp cond_id;
            let (body_id, body_cmds) = rec_tac_gen body in
            free_temp body_id;

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

            (self_id, condition @ body_ @ [label_merge] @ [TAC_int (self_id, 0)])
        | Block            { body } ->
            let rec rec_gen exprs =
                match exprs with
                | x :: [] ->
                    rec_tac_gen x
                | x :: xs ->
                    let x_id, x_cmds = rec_tac_gen x in
                    free_temp x_id;
                    let id, cmds = rec_gen xs in
                    id, x_cmds @ cmds
                | [] -> failwith "Empty block!"
            in

            rec_gen body
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

            free_temp expr_id;

            let self_id = temp_id () in

            begin match expr._type with
            (* isvoid on intrinsic types is always false *)
            | "String" | "Int" | "Bool" -> 
                (self_id, expr_cmds @ [TAC_bool (self_id, false)])
            | _ -> 
                (self_id, expr_cmds @ [TAC_isvoid (self_id, expr_id)])
            end
        | BinOp             { left; right; op } ->
            let ambigious_compare (left_id : tac_id) (right_id : tac_id) : tac_id * tac_cmd list =

                let l_val, l_cmds = cast_val left_id left._type "Object" in
                let r_val, r_cmds = cast_val right_id right._type "Object" in

                let cmp_id = temp_id () in
                let cmp = TAC_call (cmp_id, "ambigious_compare", [l_val; r_val]) in

                cmp_id, l_cmds @ r_cmds @ [cmp]
            in

            let (lhs_id, lhs_cmds) = rec_tac_gen left in
            let (rhs_id, rhs_cmds) = rec_tac_gen right in

            free_temp lhs_id;
            free_temp rhs_id;

            let self_id = temp_id () in

            let cmds = match op with
                | Plus      -> [ TAC_add (self_id, lhs_id, rhs_id) ]
                | Minus     -> [ TAC_sub (self_id, lhs_id, rhs_id) ]
                | Times     -> [ TAC_mul (self_id, lhs_id, rhs_id) ]
                | Divide    -> [ TAC_div (left.ident.line_number, self_id, lhs_id, rhs_id) ]

                | LT        ->
                    begin match left._type, right._type with
                    | "Int", "Int"
                    | "Bool", "Bool"     -> [ TAC_lt (self_id, lhs_id, rhs_id) ]
                    | "String", "String" -> [ TAC_str_lt (self_id, lhs_id, rhs_id) ]

                    | _, _ ->
                        let cmp_id, cmp_cmds = ambigious_compare lhs_id rhs_id in
                        
                        let zero = temp_id () in
                        let to_cond = [
                            TAC_int (zero, 0);
                            TAC_lt  (self_id, cmp_id, zero);
                        ] in

                        lhs_cmds @ rhs_cmds @ cmp_cmds @ to_cond
                    end
                | LE        ->
                    begin match left._type, right._type with
                    | "Int", "Int"
                    | "Bool", "Bool"     -> [ TAC_lte (self_id, lhs_id, rhs_id) ]
                    | "String", "String" -> [ TAC_str_lte (self_id, lhs_id, rhs_id) ]

                    | _, _ ->
                        let cmp_id, cmp_cmds = ambigious_compare lhs_id rhs_id in
        
                        let zero = temp_id () in
                        let to_cond = [
                            TAC_int  (zero, 0);
                            TAC_lte  (self_id, cmp_id, zero);
                        ] in

                        lhs_cmds @ rhs_cmds @ cmp_cmds @ to_cond
                    end
                | EQ        -> 
                    begin match left._type, right._type with
                    | "Int", "Int"
                    | "Bool", "Bool"     -> [ TAC_eq (self_id, lhs_id, rhs_id) ]
                    | "String", "String" -> [ TAC_str_eq (self_id, lhs_id, rhs_id) ]

                    | _, _ ->
                        let cmp_id, cmp_cmds = ambigious_compare lhs_id rhs_id in
                        
                        let zero = temp_id () in
                        let to_cond = [
                            TAC_int  (zero, 0);
                            TAC_eq   (self_id, cmp_id, zero);
                        ] in

                        lhs_cmds @ rhs_cmds @ cmp_cmds @ to_cond
                    end
            in
            
            (self_id, lhs_cmds @ rhs_cmds @ cmds)
        | UnOp             { expr; op } ->
            let (expr_id, expr_cmds) = rec_tac_gen expr in

            free_temp expr_id;
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
            let id = find_symbol ident.name in

            id, []
        | Let               { bindings; _in } ->
            let tac_initialize (binding : ast_let_binding_type) : tac_cmd list =
                let id = local_id () in

                match binding with
                | LetBindingNoInit  { variable; _type } ->
                    add_symbol variable.name id _type.name;

                    [TAC_default (id, _type.name)]
                | LetBindingInit    { variable; _type; value } ->
                    let id = local_id () in

                    let (rhs_id, rhs_cmds) = rec_tac_gen value in
                    let (casted_id, casted_cmds) = cast_val rhs_id value._type _type.name in

                    add_symbol variable.name id _type.name;

                    free_temp casted_id;

                    TAC_default (id, _type.name) :: 
                    rhs_cmds @ 
                    casted_cmds @ 
                    [TAC_ident (id, casted_id)]
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
            let case_memory = local_id () in

            let (expr_id, expr_cmds) = rec_tac_gen expression in
            let expr_cmds = expr_cmds @ [TAC_ident (case_memory, expr_id)] in

            let merge_val = temp_id () in
            let type_name = temp_id () in
            let cond = temp_id () in
            let str = temp_id () in

            free_temp cond;
            free_temp str;

            let case_label = label_id () ^ "_case" in
            let merge_label = label_id () ^ "_case_merge" in

            let type_cmd = 
                match expression._type with
                | "Int" | "String" | "Bool" -> 
                    let method_name = method_name_gen (expression._type) "type_name" in

                    [ TAC_call (type_name, method_name, []) ]
                | _ ->
                    [
                        TAC_void_check (expr.ident.line_number, case_memory, "error_case_void");
                        TAC_dispatch { 
                            line_number = expr.ident.line_number;
                            store = type_name; 
                            obj = case_memory;
                            method_id = 2; (* type_name *)
                            args = [case_memory];
                        }
                    ]
            in

            let cmds = expr_cmds @ type_cmd in

            let jmp_cmds (i : int) (mapping : ast_case_mapping) : tac_cmd list =
                let trace = [mapping._type.name] @ get_subtypes data mapping._type.name in

                match mapping._type.name with
                    | "Object" -> [TAC_jmp (case_label ^ string_of_int i)]
                    | name ->
                        trace
                        |> List.map (
                            fun _type ->
                                [
                                    TAC_str (str, _type);
                                    TAC_str_eq (cond, type_name, str);
                                    TAC_bt (cond, case_label ^ string_of_int i)
                                ]
                            ) 
                        |> List.concat
            in

            let body_cmds (i : int) (mapping : ast_case_mapping) : tac_cmd list =
                let label = TAC_label (case_label ^ string_of_int i) in

                let casted_id, casted_cmds = cast_val case_memory expression._type mapping._type.name in
                add_symbol mapping.name.name casted_id mapping._type.name;
                let body_id, body_cmds = rec_tac_gen mapping.maps_to in
                remove_symbol mapping.name.name;

                let merge_type = expr._type in
                let cast_body_id, cast_body_cmds = cast_val body_id mapping.maps_to._type merge_type in

                let merge = [
                    TAC_ident (merge_val, casted_id);
                    TAC_jmp merge_label
                ] in

                label :: casted_cmds @ body_cmds @ cast_body_cmds @ merge
            in

            let jumps, bodies = 
                mapping_list
                |> List.sort (fun (m1 : ast_case_mapping) (m2 : ast_case_mapping) -> 
                    compare
                        (type_order data m1._type.name)
                        (type_order data m2._type.name)
                    )
                |> List.rev
                |> List.mapi (fun (i : int) (mapping : ast_case_mapping) ->
                    (jmp_cmds i mapping), (body_cmds i mapping)
                )
                |> List.split
            in

            let match_fail = [
                TAC_inline_assembly ("movq\t  $" ^ string_of_int expr.ident.line_number ^ ", %rsi");
                TAC_jmp "error_case_unmatched";
            ] in

            (merge_val, cmds @ List.concat jumps @ match_fail @ List.concat bodies @ [TAC_label merge_label])
        | Internal         _ -> 
            failwith "TAC_internal should not be reachable from tac_gen_expr_body"
    in

    let (tac_id, tac_cmds) = rec_tac_gen method_body in
    let (casted_id, casted_cmds) = cast_val tac_id method_body._type return_type in

    free_temp casted_id;
    
    global_temp_counter := max !global_temp_counter !temp_counter;

    casted_id, (tac_cmds @ casted_cmds)