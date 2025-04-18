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

type ssa_expr_body = {
    end_val : ssa_id;
    stmts : ssa_stmt list;
}

let ssa_from_expr   (data : program_data) (class_name : string) (return_type : string)
                    (method_body : ast_expression)
                    (id_count : int ref) (symbol_table : ssa_sym_table ref) : ssa_expr_body =
    let stmts : ssa_stmt list ref = ref [] in

    let get_id () : ssa_id =
        let id = !id_count in
        id_count := !id_count + 1;
        SSA_id id
    in

    let add_symbol (ident : string) (id : ssa_id) (_type : string) : unit =
        StringTbl.add !symbol_table ident { id; _type }
    in

    let get_symbol (ident : string) : ssa_id =
        match StringTbl.find_opt !symbol_table ident with
        | Some sym -> sym.id
        | None -> raise (Invalid_argument ("Symbol not found: " ^ ident))
    in

    let get_symbol_type (ident : string) : string =
        match StringTbl.find_opt !symbol_table ident with
        | Some sym -> sym._type
        | None -> raise (Invalid_argument ("Symbol not found: " ^ ident))
    in

    let add_valued_statement (op : ssa_op) : ssa_id =
        let id = get_id () in
        stmts := SSA_Valued (id, op) :: !stmts;  

        id
    in

    let add_valueless_statement (stmt : ssa_op) : unit =
        stmts := SSA_Valueless stmt :: !stmts;
    in

    let cast_val (id : ssa_id) (from_type : string) (to_type : string) : ssa_id =
        match from_type, to_type with
        | "Int", "Object" ->
            add_valued_statement @@ SSA_call { method_name = "unlift_int"; args = [id] }
        | "String", "Object" ->
            add_valued_statement @@ SSA_call { method_name = "unlift_string"; args = [id] }
        | "Bool", "Object" ->
            add_valued_statement @@ SSA_call { method_name = "unlift_bool"; args = [id] }

        | "Object", "Int"
        | "Object", "String"
        | "Object", "Bool" ->
            add_valued_statement @@ SSA_call { method_name = "lift_value"; args = [id] }

        | _ -> id
    in

    let can_use_static_dispatch (_type : string) : bool =
        _type <> "SELF_TYPE" && not @@ StringSet.mem _type data.overriden_classes
    in

    let rec parse_expr (expr : ast_expression) : ssa_id =
        let gen_args (arg_types : string list) (args : ast_expression list) : ssa_id list =
            if (List.length arg_types) <> (List.length args) then
                raise (Invalid_argument "Argument count mismatch")
            else
            
            List.combine args arg_types
            |> List.mapi (
                fun (i : int) (arg, _type) ->
                    let arg_id = parse_expr arg in
                    cast_val arg_id arg._type _type
                )
        in

        let gen_void_check (id : ssa_id) (_type : string) : unit =
            match _type with
            | "Int" | "String" | "Bool" -> ()
            | _ ->
                let is_void = add_valued_statement @@ SSA_is_zero { _val = id } in
                add_valueless_statement @@ SSA_bt { _val = is_void; label = "error_void" }
        in

        let is_lifted (obj_type : string) : bool =
            match obj_type with
            | "Int" | "String" | "Bool" -> true
            | _ -> false
        in

        let gen_static_dispatch (method_name : string) (self_type : string) (self_id : ssa_id) (args : ssa_id list) : ssa_id =
            match method_name with
            | "copy"        when is_lifted self_type -> 
                self_id
            | "type_name"   when is_lifted self_type ->
                add_valued_statement @@ SSA_static (obj_name_mem_gen self_type)
            | _ ->
                add_valued_statement @@ SSA_call { method_name; args = self_id :: args }
        in

        match expr.data with 
        | Assign            { var; rhs } ->
            let var_type = get_symbol_type var.name in

            let rhs_id = parse_expr rhs in
            let casted_id = cast_val rhs_id rhs._type var_type in

            add_symbol var.name casted_id var_type;
            casted_id
        | DynamicDispatch   { call_on; _method; args } ->
            let call_on_type = if call_on._type = "SELF_TYPE" then class_name else call_on._type in
            let return_type, arg_types = get_method_signature data call_on_type _method.name in

            let args_ids = gen_args arg_types args in
            let obj_id = parse_expr call_on in

            gen_void_check obj_id return_type;

            if can_use_static_dispatch call_on_type then
                gen_static_dispatch _method.name call_on_type obj_id args_ids
            else
                let method_id = get_dispatch data class_name _method.name in
                add_valued_statement @@ SSA_dispatch { obj = obj_id; method_id; args = args_ids }
        | StaticDispatch    { call_on; _method; args } ->
            let return_type, arg_types = get_method_signature data call_on._type _method.name in

            let args_ids = gen_args arg_types args in
            let obj_id = parse_expr call_on in

            gen_void_check obj_id return_type;
            gen_static_dispatch _method.name call_on._type obj_id args_ids
        | SelfDispatch      { _method; args } ->
            let return_type, arg_types = get_method_signature data class_name _method.name in

            let args_ids = gen_args arg_types args in
            let obj_id = get_symbol "self" in

            if can_use_static_dispatch class_name then
                gen_static_dispatch _method.name class_name obj_id args_ids
            else
                let method_id = get_dispatch data class_name _method.name in
                add_valued_statement @@ SSA_dispatch { obj = obj_id; method_id; args = args_ids }
        | BinOp             { left; right; op } ->
            let lhs_id = parse_expr left in
            let rhs_id = parse_expr right in

            let op_type = match op with
                | Plus      -> SSA_add 
                | Minus     -> SSA_sub
                | Times     -> SSA_mul
                | Divide    -> SSA_div
                
                | LT        -> SSA_lt
                | LE        -> SSA_lte
                | EQ        -> SSA_eq
            in

            add_valued_statement @@ SSA_bin_op { _type = op_type; lhs = lhs_id; rhs = rhs_id }
        | UnOp              { expr; op } ->
            let arg_id = parse_expr expr in
            let op_type = match op with
                | Negate    -> SSA_neg
                | Not       -> SSA_not
            in

            add_valued_statement @@ SSA_un_op { _type = op_type; lhs = arg_id; rhs = arg_id }
        | String            s ->
            add_valued_statement @@ SSA_str s
        | Integer           i ->
            add_valued_statement @@ SSA_int i
        | _ -> failwith "Unsupported expression type"
    in

    let end_val = parse_expr method_body in
    let casted = cast_val end_val method_body._type return_type in
    let stmts = List.rev !stmts in

    { end_val = casted; stmts }
