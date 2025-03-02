open E_ast_data
open E_symbol_map
open D_ast

module StringMap = Map.Make(String)

type output_context = {
    classes : class_map;
    vars : symbol_map;
    current_class : string;
}

let rec get_expr_type (context : output_context) (expr : ast_expression) : string =
    match expr.data with
    | Assign { rhs; _ } -> get_expr_type context rhs
    | DynamicDispatch { call_on; _method; _ } -> 
        let class_name = get_expr_type context call_on in
        let dispatch = (get_dispatch context.classes class_name _method.name) in
        Printf.printf "Dynamic Dispatch: %s\n" _method.name;
        (Option.get dispatch)._type.name
    | StaticDispatch { call_on; _type; _method; _ } -> 
        Printf.printf "Static Dispatch: %s.%s\n" _type.name _method.name;
        let dispatch = (get_static_dispatch context.classes _type.name _method.name) in
        (Option.get dispatch)._type.name
    | SelfDispatch { _method; _ } -> 
        Printf.printf "Self Dispatch: %s\n" _method.name;
        let dispatch = (get_dispatch context.classes context.current_class _method.name) in
        (Option.get dispatch)._type.name
    | If { _then; _else; _ } -> 
        let then_type = get_expr_type context _then in
        let else_type = get_expr_type context _else in

        join_classes context.classes then_type else_type
    | While { predicate; _ } -> "Object"
    | Block { body } ->
        let rec last_type (l : ast_expression list) =
            match l with
            | [] -> raise (Failure "Empty block")
            | x :: [] -> get_expr_type context x
            | _ :: xs -> last_type xs
        in

        last_type body
    | New { _class } -> _class.name
    | IsVoid { expr } -> "Bool"
    | BinOp { op } -> 
        (match op with
        | Plus | Minus | Times | Divide -> "Int"
        | LT | LE | EQ -> "Bool"
        )
    | UnOp { op } ->
        (match op with
        | Not -> "Bool"
        | Negate -> "Int"
        )
    | Integer _ -> "Int"
    | String _ -> "String"
    | Identifier ident -> 
        if ident.name = "self" then "SELF_TYPE"
        else get_symbol ident.name context.vars
    | True | False -> "Bool"
    | Let { bindings; _in } -> "Object"
    | True -> "Bool"
    | False -> "Bool"
    | Case { mapping_list } -> 
        let types = List.map (fun (x : ast_case_mapping) -> x._type.name) mapping_list in
        List.fold_left (fun x y -> join_classes context.classes x y) (List.hd types) (List.tl types)
    | Unit -> "Object"
    | Unreachable -> "Object"
    | Internal -> "Object"

let output_ast (ast : ast_data) (file_path : string) : unit =
    let oc = open_out file_path in

    let output_line (line : string) = Printf.fprintf oc "%s\n" line in
    let output_number (num : int) = Printf.fprintf oc "%d\n" num in
    let output_identifier (ident : D_ast.ast_identifier) =
        output_number ident.line_number;
        output_line ident.name;
    in
    let output_typed_identifier (ident : D_ast.ast_identifier) (type_ : string) =
        output_number ident.line_number;
        output_line type_;
        output_line ident.name;
    in
    let output_list (l: 'a list) (for_each : 'a -> unit) =  
        output_number (List.length l);
        List.iter for_each l
    in
    let symbol_table = new_symbol_map () in

    let rec output_expression (expr : ast_expression) (context : output_context) : unit =
        output_typed_identifier   expr.ident (get_expr_type context expr);

        match expr.data with
        | Assign            { var; rhs } ->
            output_identifier   var;
            output_expression   rhs context;
        | DynamicDispatch   { call_on; _method; args } ->
            output_expression   call_on context;
            output_identifier   _method;
            output_list         args    (fun x -> output_expression x context)
        | StaticDispatch    { call_on; _type; _method; args } ->
            output_expression call_on context;
            output_identifier _type;
            output_identifier _method;
            output_list         args    (fun x -> output_expression x context)
        | SelfDispatch      { _method; args } ->
            output_identifier   _method;            
            output_list         args    (fun x -> output_expression x context)
        | If                { predicate; _then; _else } ->
            output_expression   predicate context;
            output_expression   _then    context;
            output_expression   _else   context
        | While             { predicate; body } -> 
            output_expression   predicate context;
            output_expression   body    context
        | Block             { body } ->
            output_list         body    (fun x -> output_expression x context)
        | New               { _class } ->
            output_identifier   _class
        | IsVoid            { expr } ->
            output_expression   expr context
        | BinOp             { left; right; _ } ->
            output_expression   left context;
            output_expression   right context;
        | UnOp              { expr; _ } ->
            output_expression   expr context
        | Integer i  ->
            output_number       i
        | String str ->
            output_line         str
        | Identifier ident ->
            output_identifier   ident
        | True -> ()
        | False -> ()
        | Let               { bindings; _in } ->
            let output_binding (binding : D_ast.ast_let_binding_type) : unit =
                (match binding with 
                | LetBindingNoInit      { variable; _type } ->
                    output_line         "let_binding_no_init";
                    output_identifier   variable;
                    output_identifier   _type;
                    output_expression   _in { context with vars = add_symbol variable.name _type.name context.vars }
                | LetBindingInit        { variable; _type; value } ->
                    output_line         "let_binding_init";
                    output_identifier   variable;
                    output_identifier   _type;
                    output_expression   value context;
                    output_expression   _in { context with vars = add_symbol variable.name _type.name context.vars })
            in

            output_number       (List.length bindings);
            List.iter           output_binding bindings
        | Case                  { expression; mapping_list } ->
            output_expression   expression;

            let output_case_mapping (mapping : D_ast.ast_case_mapping) =
                output_identifier   mapping.name;
                output_identifier   mapping._type;
                output_expression   mapping.maps_to { context with vars = add_symbol mapping.name.name mapping._type.name context.vars }
            in

            output_number       (List.length mapping_list);
            List.iter           output_case_mapping mapping_list
        | Unreachable -> output_line "unreachable"
        | Internal    -> output_line "internal"
        | _ -> Printf.printf "Unhandled Expression!\n"; exit 1
    in

    let output_class_map () =
        output_line "class_map";
        output_number (StringMap.cardinal ast.classes);

        let context = { classes = ast.classes; vars = new_symbol_map (); current_class = "" } in

        let output_class _ (class_data : E_ast_data.class_data) : unit =
            let output_attribute (_attr : D_ast.ast_attribute) : unit =
                match _attr with
                | AttributeNoInit   { name; _type } ->
                        output_line "no_initializer";
                        output_line name.name;
                        output_line _type.name
                | AttributeInit     { name; _type; init } ->
                        output_line "initializer";
                        output_line name.name;
                        output_line _type.name;
                        output_expression init context
            in

            let attributes = E_ast_data.get_attributes ast.classes class_data.class_ref.name.name in

            output_line class_data.class_ref.name.name;
            output_number (List.length attributes);
            List.iter output_attribute attributes
        in

        StringMap.iter output_class ast.classes
    in

    let output_implementation_map () =
        let context = { classes = ast.classes; vars = symbol_table; current_class = "" } in

        let output_class _ (class_data : class_data) : unit =
            let output_parameters (params : ast_param list) : unit =
                let output_parameter (param : ast_param) : unit =
                    output_line param.name.name;
                    output_line param._type.name
                in

                output_number (List.length params);
                List.iter output_parameter params
            in
            
            let output_method (_method : ast_method) : unit =
                output_line _method.name.name;
                output_parameters _method.params;
                output_line _method._type.name;
                output_expression _method.body { context with current_class = class_data.class_ref.name.name }
            in

            let _class = class_data.class_ref in

            output_line _class.name.name;
            output_number (List.length _class.methods);
            List.iter output_method _class.methods
        in

        output_line "implementation_map";
        output_number (StringMap.cardinal ast.classes);
        StringMap.iter output_class ast.classes
    in

    let output_parent_map () =
        let output_class _ (class_data : class_data) : unit =
            let name = class_data.class_ref.name.name in

            if name <> "Object" then (
                output_line name;
                match class_data.class_ref.inherits with
                | Some parent -> output_line parent.name
                | None -> output_line "Object"
            )
        in

        output_line "parent_map";
        (* Don't include Object *)
        output_number ((StringMap.cardinal ast.classes) - 1);
        StringMap.iter output_class ast.classes
    in

    output_parent_map ()