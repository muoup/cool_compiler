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

    let gen_id () : ssa_id =
        let id = Local !local_counter in
        local_counter := !local_counter + 1;
        ssa_id_list := id :: !ssa_id_list;
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

    let rec rec_ssa_gen (expr : ast_expression) : (ssa_id * ssa_stmt list) =
        let gen_args (args : ast_expression list) : (ssa_id list * ssa_stmt list list) =
            let (args_ids, args_cmds) = List.split (List.map rec_ssa_gen args) in
            let args_ids = args_ids in

            args_ids, args_cmds
        in

        match expr.data with
        | Assign            { var; rhs } ->
            let var_id = find_symbol var.name in
            let (rhs_id, rhs_cmds) = rec_ssa_gen rhs in

            let assign_cmd = SSA_store (var_id, rhs_id) in

            (var_id, rhs_cmds @ [assign_cmd])
        | DynamicDispatch   { call_on; _method; args } ->
            let (obj_id, obj_cmds) = rec_ssa_gen call_on in
            let (args_ids, args_cmds) = gen_args args in
            let self_id = gen_id () in

            let comment = SSA_comment ("DynamicDispatch: " ^ _method.name) in

            if not (StringSet.mem call_on._type data.overriden_classes) then
                let dispatch = method_name_gen call_on._type _method.name in

                let call_cmd = SSA_call (self_id, dispatch, obj_id :: args_ids) in
                (self_id, obj_cmds @ List.concat args_cmds @ [comment; call_cmd])
            else

            let method_id = get_dispatch data call_on._type _method.name in

            let call_cmd = SSA_dispatch { 
                line_number = _method.line_number;
                store = self_id; 
                obj = obj_id;
                method_id;
                args = obj_id :: args_ids;
            } in
            (self_id, obj_cmds @ (List.concat args_cmds @ [comment; call_cmd]))
        | StaticDispatch    { call_on; _type; _method; args; } ->
            let (obj_id, obj_cmds) = rec_ssa_gen call_on in
            let (args_ids, args_cmds) = List.split (List.map rec_ssa_gen args) in
            let self_id = gen_id () in

            let comment = SSA_comment ("StaticDispatch: " ^ _type.name ^ "." ^ _method.name) in
            let dispatch = method_name_gen _type.name _method.name in

            let call_cmd = SSA_call (self_id, dispatch, obj_id :: args_ids) in
            (self_id, obj_cmds @ List.concat args_cmds @ [comment; call_cmd])
        | SelfDispatch      { _method; args } ->
            let (args_ids, args_cmds) = gen_args args in
            let self_id = gen_id () in
            let comment = SSA_comment ("SelfDispatch: " ^ _method.name) in

            if not (StringSet.mem class_name data.overriden_classes) then
                let dispatch = method_name_gen class_name _method.name in

                let call_cmd = SSA_call (self_id, dispatch, Self :: args_ids) in
                (self_id, List.concat args_cmds @ [comment; call_cmd])
            else

            let dispatch = get_dispatch data class_name _method.name in

            let call_cmd = SSA_dispatch {
                line_number = _method.line_number;
                store = self_id; 
                obj = Self;
                method_id = dispatch;
                args = Self :: args_ids;
            } in
            
            (self_id, List.concat args_cmds @ [comment; call_cmd])
        | If                { predicate; _then; _else } ->
            let (cond_id, cond_cmds) = rec_ssa_gen predicate in
            let (then_id, then_cmds) = rec_ssa_gen _then in
            let (else_id, else_cmds) = rec_ssa_gen _else in

            let self_id = gen_id () in

            let then_name = label_id () ^ "_then" in
            let else_name = label_id () ^ "_else" in
            let merge_name = label_id () ^ "_merge" in

            let bt_cmd = SSA_bt (cond_id, then_name) in
            let jmp_cmd = SSA_jmp else_name in

            let label_then  = SSA_label then_name in
            let label_else  = SSA_label else_name in
            let label_merge = SSA_label merge_name in

            let condition = cond_cmds @ [bt_cmd; jmp_cmd] in
            let then_ = [label_then] @ then_cmds @ [SSA_ident (self_id, then_id)] @ [SSA_jmp merge_name] in
            let else_ = [label_else] @ else_cmds @ [SSA_ident (self_id, else_id)] @ [label_merge] in

            (self_id, condition @ then_ @ else_)
        | While             { predicate; body } ->
            let (cond_id, cond_cmds) = rec_ssa_gen predicate in
            let (body_id, body_cmds) = rec_ssa_gen body in

            let self_id = gen_id () in

            let cond_name = label_id () ^ "_cond" in
            let body_name = label_id () ^ "_body" in
            let merge_name = label_id () ^ "_merge" in

            let bt_cmd = SSA_bt (cond_id, body_name) in
            let jmp_cmd = SSA_jmp merge_name in

            let label_body = SSA_label body_name in
            let label_cond = SSA_label cond_name in
            let label_merge = SSA_label merge_name in

            let condition = [label_cond] @ cond_cmds @ [bt_cmd; jmp_cmd] in
            let body_ = label_body :: body_cmds @ [SSA_jmp cond_name] in

            (self_id, condition @ body_ @ [label_merge])
        | Block            { body } ->
            let (ids, cmds) = List.split (List.map rec_ssa_gen body) in

            (last_id ids, List.concat cmds)
        | New              { _class } ->
            let self_id = gen_id () in

            let _type = if _class.name = "SELF_TYPE" then class_name else _class.name in

            (self_id, [SSA_new (self_id, _type)])
        | IsVoid           { expr } ->
            let (expr_id, expr_cmds) = rec_ssa_gen expr in
            let self_id = gen_id () in

            (self_id, expr_cmds @ [SSA_isvoid (self_id, expr_id)])
        | BinOp             { left; right; op } ->
            let (lhs_id, lhs_cmds) = rec_ssa_gen left in
            let (rhs_id, rhs_cmds) = rec_ssa_gen right in
            let self_id = gen_id () in

            let cmd = match op with
                | Plus      -> SSA_add (self_id, lhs_id, rhs_id)
                | Minus     -> SSA_sub (self_id, lhs_id, rhs_id)
                | Times     -> SSA_mul (self_id, lhs_id, rhs_id)
                | Divide    -> SSA_div (left.ident.line_number, self_id, lhs_id, rhs_id)

                | LT        -> SSA_lt  (self_id, lhs_id, rhs_id)
                | LE        -> SSA_lte (self_id, lhs_id, rhs_id)
                | EQ        -> 
                    match left._type with
                    | "String" -> SSA_str_eq (self_id, lhs_id, rhs_id)
                    | _        -> SSA_eq (self_id, lhs_id, rhs_id)
            in
            
            (self_id, lhs_cmds @ rhs_cmds @ [cmd])
        | UnOp             { expr; op } ->
            let (expr_id, expr_cmds) = rec_ssa_gen expr in
            let self_id = gen_id () in

            let cmd = match op with
                | Not       -> SSA_not (self_id, expr_id)
                | Negate    -> SSA_neg (self_id, expr_id)
            in

            (self_id, expr_cmds @ [cmd])
        | Integer           i ->
            let self_id = gen_id () in 
            (self_id, [SSA_ident (self_id, IntLiteral i)])
        | String            s ->
            let escaped_s = escape_backslashes s in
            let self_id = gen_id () in
            (self_id, [SSA_ident (self_id, StringLiteral escaped_s)])
        | True                -> 
            let self_id = gen_id () in
            (self_id, [SSA_ident (self_id, BoolLiteral true)])
        | False               ->
            let self_id = gen_id () in
            (self_id, [SSA_ident (self_id, BoolLiteral false)])
        | Identifier        ident ->
            let var_name = find_symbol ident.name in
            let self_id = gen_id () in

            (self_id, [SSA_load (self_id, var_name)])
        | Let               { bindings; _in } ->
            let ssa_initialize (binding : ast_let_binding_type) : ssa_stmt list =
                match binding with
                | LetBindingNoInit  { variable; _type } ->
                    let id = gen_id() in
                    add_symbol variable.name id;
                    [SSA_default_mem (id, _type.name)]
                | LetBindingInit    { variable; _type; value } ->
                    let id = gen_id () in
                    let (rhs_id, rhs_cmds) = rec_ssa_gen value in
                    add_symbol variable.name rhs_id;
                    rhs_cmds @ [SSA_valued_mem (id, rhs_id, _type.name)]
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
            (gen_id (), [SSA_comment "Case not implemented"])
        | Internal         _ -> 
            (gen_id (), [SSA_comment "Internal expression not implemented"])
    in

    let (ssa_id, ssa_cmds) = rec_ssa_gen method_body in
    (!ssa_id_list, ssa_cmds @ [SSA_return ssa_id])