open E_ast_data
open E_symbol_map
open D_ast

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)


type class_member =
    | Method        of ast_method
    | Attribute     of ast_attribute

type output_context = {
    classes : class_map;
    vars : symbol_map;

    current_method : string;
    current_class : string;
}

let get_member_line (member : class_member) : int =
    match member with
    | Method _method -> _method.name.line_number
    | Attribute _attr ->
        (match _attr with
        | AttributeNoInit { name; _type } -> name.line_number
        | AttributeInit { name; _type; init } -> name.line_number)

let rec get_expr_type (context : output_context) (expr : ast_expression) : string =
    match expr.data with
    | Assign { rhs; _ } -> get_expr_type context rhs
    | DynamicDispatch { call_on; _method; _ } -> 
        let class_name = get_expr_type context call_on in
        let dispatch = (get_dispatch context.classes class_name _method.name) in
        (Option.get dispatch)._type.name
    | StaticDispatch { call_on; _type; _method; _ } -> 
        let dispatch = (get_static_dispatch context.classes _type.name _method.name) in
        (Option.get dispatch)._type.name
    | SelfDispatch { _method; _ } -> 
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
    | Case { mapping_list } -> 
        let types = List.map (fun (x : ast_case_mapping) -> x._type.name) mapping_list in
        List.fold_left (fun x y -> join_classes context.classes x y) (List.hd types) (List.tl types)
    | Unit -> "Object"
    | Unreachable -> "Object"
    | Internal _ ->
        let _method = get_dispatch context.classes context.current_class context.current_method in

        (Option.get _method)._type.name

let rec get_attributes (context : output_context) (class_name : string) : ast_attribute list =
    let _class = StringMap.find class_name context.classes in
    let self_attributes =
        List.map snd @@ StringMap.bindings _class.attributes
    in

    match _class.class_ref.inherits with
    | None -> self_attributes
    | Some inherit_from -> get_attributes context inherit_from.name @ self_attributes

let get_methods (context : output_context) (class_name : string) : (string * string) list =
    let rec get_unique_methods (class_data : class_data) (methods : StringSet.t) : (string * string list) list =
        let unique_methods = List.filter
            (fun (_method : ast_method) -> not (StringSet.mem _method.name.name methods))
            (List.map snd @@ StringMap.bindings class_data.methods)
        in

        let unique_names = List.map
            (fun (_method : ast_method) -> _method.name.name)
            unique_methods
        in

        let methods = List.fold_left
            (fun methods name -> StringSet.add name methods)
            methods
            unique_names
        in

        if class_data.class_ref.name.name = "Object" then [("Object", unique_names)] else

        let parent = match class_data.class_ref.inherits with
        | Some parent -> parent.name
        | None -> "Object"
        in

        let object_data = StringMap.find parent context.classes in
        (class_data.class_ref.name.name, unique_names) :: (get_unique_methods object_data methods)
    in

    let unique_methods = get_unique_methods (StringMap.find class_name context.classes) StringSet.empty in
    let as_method_list = List.map
        (fun (class_name, methods) -> List.map (fun _method -> (class_name, _method)) methods)
        (List.rev unique_methods)
    in

    List.flatten as_method_list

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
            let context = { context with vars = push_scope context.vars } in
            output_expression   _then    context;
            let context = { context with vars = pop_scope context.vars } in
            let context = { context with vars = push_scope context.vars } in
            output_expression   _else   context
        | While             { predicate; body } -> 
            output_expression   predicate context;
            let context = { context with vars = push_scope context.vars } in
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
            output_expression   expression context;

            let output_case_mapping (mapping : D_ast.ast_case_mapping) =
                output_identifier   mapping.name;
                output_identifier   mapping._type;
                output_expression   mapping.maps_to { context with vars = add_symbol mapping.name.name mapping._type.name context.vars }
            in

            output_number       (List.length mapping_list);
            List.iter           output_case_mapping mapping_list
        | Unreachable -> ()
        | Internal data -> 
            output_line data
        | _ -> Printf.printf "Unhandled Expression!\n"; exit 1
    in

    let output_parameters (params : ast_param list) : unit =
        output_number (List.length params);
        List.iter (fun (param : ast_param) -> output_line param.name.name) params
    in

    let output_method (context : output_context) (class_name : string) (method_name : string) : unit =
        let _method = StringMap.find method_name (StringMap.find class_name ast.classes).methods in
        let context = { context with current_method = method_name } in

        output_line method_name;
        output_parameters _method.params;
        output_line class_name;        
        output_expression _method.body { context with current_class = class_name };
    in

    let output_class_map (context : output_context) : unit =
        output_line "class_map";
        output_number (StringMap.cardinal ast.classes);

        let output_attribute (_attr : ast_attribute) (context : output_context) : unit =
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

        let output_class _ (class_data : class_data) : unit =
            let context = { context with current_class = class_data.class_ref.name.name } in
            let attributes = get_attributes context class_data.class_ref.name.name in

            output_line class_data.class_ref.name.name;
            output_number (List.length attributes);
            List.iter (fun _attr -> output_attribute _attr context) attributes
        in

        StringMap.iter output_class context.classes
    in

    let output_implementation_map (context : output_context) =
        let output_class _ (class_data : class_data) : unit =
            let context = { context with current_class = class_data.class_ref.name.name } in

            let _class = class_data.class_ref in
            let ordered_methods = get_methods context _class.name.name in

            output_line _class.name.name;
            output_number @@ List.length ordered_methods;

            List.iter
                (fun (class_name, _method) -> output_method context class_name _method)
                ordered_methods
        in

        output_line "implementation_map";
        output_number (StringMap.cardinal ast.classes);
        StringMap.iter output_class ast.classes
    in

    let output_parent_map (context : output_context) =
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

    let output_annotated_ast (context : output_context) =
        let output_class (class_data : class_data) =
            let context = { context with current_class = class_data.class_ref.name.name } in
            output_identifier class_data.class_ref.name;

            (match class_data.class_ref.inherits with
            | Some parent -> 
                output_line "inherits";
                output_identifier parent
            | None -> ()
            );

            let output_body_expr (member : class_member) =
                match member with
                | Method _method -> 
                    output_line "method";
                    output_identifier _method.name;
                    output_parameters _method.params;
                    output_identifier _method._type;

                    let context = { context with current_method = _method.name.name } in
                    output_expression _method.body context
                | Attribute attr ->
                    (match attr with
                    | AttributeNoInit { name; _type } ->
                        output_line "attribute_no_init";
                        output_identifier name;
                        output_identifier _type
                    | AttributeInit { name; _type; init } ->
                        output_line "attribute_init";
                        output_identifier name;
                        output_identifier _type;
                        output_expression init context)
            in

            let method_members = List.map 
                (fun (_method : ast_method) -> Method _method) 
                class_data.class_ref.methods
            in
            let attribute_members = List.map 
                (fun (_attr : ast_attribute) -> Attribute _attr) 
                class_data.class_ref.attributes
            in
            let body_members = method_members @ attribute_members in

            (* 
                I simply don't have the time to go back and reformat the ast to include both methods and
                attributes together, so this is a hacky way to get them back in order.
            *)
            let sorted = List.sort
                (fun body1 body2 ->
                    let line1 = get_member_line body1 in
                    let line2 = get_member_line body2 in

                    compare line1 line2
                )
                body_members 
            in

            output_number (List.length sorted);
            List.iter output_body_expr sorted
        in

        let output_filter (class_data : class_data) =
            match class_data.class_ref.name.name with
            | "Object" | "IO" | "Int" | "String" | "Bool" -> ()
            | _ -> output_class class_data
        in

        (* Exclude 5 intrinsic class types *)
        output_number (StringMap.cardinal ast.classes - 5);
        StringMap.iter (fun _ class_data -> output_filter class_data) ast.classes
    in

    let context = { classes = ast.classes; vars = new_symbol_map (); current_class = ""; current_method = "" } in

    output_class_map context;
    output_implementation_map context;
    output_parent_map context;
    output_annotated_ast context