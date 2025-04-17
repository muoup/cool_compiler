open A_util
open B_ast
open B_class_map
open B_impl_map
open B_parent_map
open C_parser_data
open D_ssa_data

let label_id : int ref = ref 0

let label_id () : string =
    let id = !label_id in
    label_id := !label_id + 1;
    Printf.sprintf "L%d" id

let rec last_id (ids : ssa_id list) : ssa_id =
    match ids with
    | [] -> raise (Invalid_argument "Empty list")
    | [id] -> id
    | _ :: tl -> last_id tl

let ssa_gen_expr_body (data : program_data) (class_name : string) (method_body : ast_expression) (symbol_table : symbol_table ref) (temp_counter : int ref) (local_counter : int ref) : (ssa_id list * ssa_stmt list) =
    let ssa_id_list : ssa_id list ref = ref [] in

    let create_stmt (_val : ssa_val) : ssa_stmt =
        let id = Local !temp_counter in
        temp_counter := !temp_counter + 1;
        { id; _val }
    in

    let add_symbol (x : string) (id : ssa_id) : unit =
        StringTbl.add !symbol_table x id;
        ssa_id_list := id :: !ssa_id_list;
    in

    let remove_symbol (x : string) : unit =
        StringTbl.remove !symbol_table x
    in

    let find_symbol (x : string) : ssa_id =
        match StringTbl.find_opt !symbol_table x with
        | Some id -> id
        | None -> raise (Invalid_argument ("Symbol not found: " ^ x))
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

    let rec rec_ssa_gen (expr : ast_expression) : (ssa_id * ssa_stmt list) =
        let gen_args (args : ast_expression list) : (ssa_id list * ssa_stmt list list) =
            let (args_ids, args_cmds) = List.split (List.map rec_ssa_gen args) in
            let args_ids = args_ids in

            args_ids, args_cmds
        in

        match expr.data with
        | Assign            { var; rhs } ->
            let (rhs_id, rhs_cmds) = rec_ssa_gen rhs in
            let var_id = find_symbol var.name in

            let assign_cmd = create_stmt @@ SSA_store (rhs_id, var_id) in

            (assign_cmd.id, rhs_cmds @ [assign_cmd])
        | DynamicDispatch   { call_on; _method; args } ->
            let (obj_id, obj_cmds) = rec_ssa_gen call_on in
            let (args_ids, args_cmds) = gen_args args in
            let comment = create_stmt @@ SSA_comment ("DynamicDispatch: " ^ _method.name) in

            if not (StringSet.mem call_on._type data.overriden_classes) then
                let dispatch = method_name_gen call_on._type _method.name in
                let call_cmd = create_stmt @@ SSA_call (dispatch, obj_id :: args_ids) in

                (call_cmd.id, obj_cmds @ List.concat args_cmds @ [comment; call_cmd])
            else

            let method_id = get_dispatch data call_on._type _method.name in
            let call_cmd = create_stmt @@ SSA_dispatch { 
                line_number = _method.line_number;
                obj = obj_id;
                method_id;
                args = obj_id :: args_ids;
            } in

            (call_cmd.id, obj_cmds @ (List.concat args_cmds @ [comment; call_cmd]))
        | StaticDispatch    { call_on; _type; _method; args; } ->
            let (obj_id, obj_cmds) = rec_ssa_gen call_on in
            let (args_ids, args_cmds) = List.split (List.map rec_ssa_gen args) in

            let comment = create_stmt @@ SSA_comment ("StaticDispatch: " ^ _type.name ^ "." ^ _method.name) in
            let dispatch = method_name_gen _type.name _method.name in
            let call_cmd = create_stmt @@ SSA_call (dispatch, obj_id :: args_ids) in

            (call_cmd.id, obj_cmds @ List.concat args_cmds @ [comment; call_cmd])
        | SelfDispatch      { _method; args } ->
            let (args_ids, args_cmds) = gen_args args in
            let comment = create_stmt @@ SSA_comment ("SelfDispatch: " ^ _method.name) in

            if not (StringSet.mem class_name data.overriden_classes) then
                let dispatch = method_name_gen class_name _method.name in
                let call_cmd = create_stmt @@ SSA_call (dispatch, Self :: args_ids) in

                (call_cmd.id, List.concat args_cmds @ [comment; call_cmd])
            else

            let dispatch = get_dispatch data class_name _method.name in
            let call_cmd = create_stmt @@ SSA_dispatch {
                line_number = _method.line_number;
                obj = Self;
                method_id = dispatch;
                args = Self :: args_ids;
            } in
            
            (call_cmd.id, List.concat args_cmds @ [comment; call_cmd])
        | If                { predicate; _then; _else } ->
            let (cond_id, cond_cmds) = rec_ssa_gen predicate in
            let (then_id, then_cmds) = rec_ssa_gen _then in
            let (else_id, else_cmds) = rec_ssa_gen _else in

            let value = create_stmt @@ SSA_default_mem "Object" in

            let then_name = label_id () ^ "_then" in
            let else_name = label_id () ^ "_else" in
            let merge_name = label_id () ^ "_merge" in

            let bt_cmd = create_stmt @@ SSA_bt (cond_id, then_name) in
            let jmp_cmd = create_stmt @@ SSA_jmp else_name in

            let label_then  = create_stmt @@ SSA_label then_name in
            let label_else  = create_stmt @@ SSA_label else_name in
            let label_merge = create_stmt @@ SSA_label merge_name in

            let condition = cond_cmds @ [bt_cmd; jmp_cmd] in
            let then_ = [label_then] @ then_cmds @ [create_stmt @@ SSA_store (then_id, value.id)] @ [create_stmt @@ SSA_jmp merge_name] in
            let else_ = [label_else] @ else_cmds @ [create_stmt @@ SSA_store (else_id, value.id)] @ [create_stmt @@ SSA_jmp merge_name] in
            
            let load = create_stmt @@ SSA_load value.id in

            (load.id, value :: condition @ then_ @ else_ @ [label_merge; load])
        | While             { predicate; body } ->
            let (cond_id, cond_cmds) = rec_ssa_gen predicate in
            let (body_id, body_cmds) = rec_ssa_gen body in

            let cond_name = label_id () ^ "_cond" in
            let body_name = label_id () ^ "_body" in
            let merge_name = label_id () ^ "_merge" in

            let bt_cmd = create_stmt @@ SSA_bt (cond_id, body_name) in
            let jmp_cmd = create_stmt @@ SSA_jmp merge_name in

            let label_body = create_stmt @@ SSA_label body_name in
            let label_cond = create_stmt @@ SSA_label cond_name in
            let label_merge = create_stmt @@ SSA_label merge_name in

            let condition = [label_cond] @ cond_cmds @ [bt_cmd; jmp_cmd] in
            let body_ = label_body :: body_cmds @ [create_stmt @@ SSA_jmp cond_name] in

            (Unit, condition @ body_ @ [label_merge])
        | Block            { body } ->
            let (ids, cmds) = List.split (List.map rec_ssa_gen body) in

            (last_id ids, List.concat cmds)
        | New              { _class } ->
            let _type = if _class.name = "SELF_TYPE" then class_name else _class.name in

            let cmd = create_stmt @@ match _type with
            | "Int" | "String" | "Bool" -> SSA_default_mem _type
            | _ -> SSA_new _type
            in

            (cmd.id, [cmd])
        | IsVoid           { expr } ->
            let (expr_id, expr_cmds) = rec_ssa_gen expr in
            let cmd = create_stmt @@ SSA_isvoid expr_id in

            (cmd.id, expr_cmds @ [cmd])
        | BinOp             { left; right; op } ->
            let (lhs_id, lhs_cmds) = rec_ssa_gen left in
            let (rhs_id, rhs_cmds) = rec_ssa_gen right in

            let cmd = create_stmt @@ match op with
                | Plus      -> SSA_add (lhs_id, rhs_id)
                | Minus     -> SSA_sub (lhs_id, rhs_id)
                | Times     -> SSA_mul (lhs_id, rhs_id)
                | Divide    -> SSA_div (left.ident.line_number, lhs_id, rhs_id)

                | LE        -> SSA_lte (lhs_id, rhs_id)
                | LT        -> SSA_lt  (lhs_id, rhs_id)
                | EQ        -> 
                    match left._type with
                    | "String" -> SSA_str_eq (lhs_id, rhs_id)
                    | _        -> SSA_eq (lhs_id, rhs_id)
            in
            
            (cmd.id, lhs_cmds @ rhs_cmds @ [cmd])
        | UnOp             { expr; op } ->
            let (expr_id, expr_cmds) = rec_ssa_gen expr in

            let cmd = create_stmt @@ match op with
                | Not       -> SSA_not (expr_id)
                | Negate    -> SSA_neg (expr_id)
            in

            (cmd.id, expr_cmds @ [cmd])
        | Integer           i ->
            let cmd = create_stmt @@ SSA_ident (IntLiteral i) in
            (cmd.id, [cmd])
        | String            s ->
            let escaped_s = escape_backslashes s in
            let cmd = create_stmt @@ SSA_ident (StringLiteral escaped_s) in
            (cmd.id, [cmd])
        | True                -> 
            let cmd = create_stmt @@ SSA_ident (BoolLiteral true) in
            (cmd.id, [cmd])
        | False               ->
            let cmd = create_stmt @@ SSA_ident (BoolLiteral false) in
            (cmd.id, [cmd])
        | Identifier        ident ->
            let var_name = find_symbol ident.name in
            let cmd = create_stmt @@ SSA_ident (var_name) in

            (cmd.id, [cmd])
        | Let               { bindings; _in } ->
            let ssa_initialize (binding : ast_let_binding_type) : ssa_stmt list =
                match binding with
                | LetBindingNoInit  { variable; _type } ->
                    let cmd = create_stmt @@ SSA_default_mem _type.name in
                    add_symbol variable.name cmd.id;
                    [cmd]
                | LetBindingInit    { variable; _type; value } ->
                    let (rhs_id, rhs_cmds) = rec_ssa_gen value in
                    let cmd = create_stmt @@ SSA_valued_mem (rhs_id, variable.name) in
                    add_symbol variable.name cmd.id;
                    rhs_cmds @ [cmd]
            in

            let ssa_remove_binding (binding : ast_let_binding_type) : unit =
                match binding with
                | LetBindingNoInit  { variable; _type } ->
                    remove_symbol variable.name
                | LetBindingInit    { variable; _type; value } ->
                    remove_symbol variable.name
            in

            let init_cmds = List.concat (List.map ssa_initialize bindings) in
            let (in_id, in_cmds) = rec_ssa_gen _in in

            List.iter (ssa_remove_binding) bindings;

            (in_id, init_cmds @ in_cmds)
        | Case              { expression; mapping_list } ->
            (Unit, [create_stmt @@ SSA_comment "Case not implemented"])
        | Internal         _ -> 
            (Unit, [create_stmt @@ SSA_comment "Internal expression not implemented"])
    in

    let (ssa_id, ssa_cmds) = rec_ssa_gen method_body in
    (!ssa_id_list, ssa_cmds @ [create_stmt @@ SSA_return ssa_id])