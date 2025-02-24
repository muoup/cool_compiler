let output_ast (ast : D_ast.ast) (file_path : string) : unit =
    let output_list (l: 'a list) (for_each : 'a -> unit) =
        List.iter for_each l
    in

    let oc = open_out file_path in

    let output_line (line : string) = Printf.fprintf oc "%s\n" line in
    let output_number (num : int) = Printf.fprintf oc "%d\n" num in
    let output_identifier (ident : D_ast.ast_identifier) =
        output_number ident.line_number;
        output_line ident.name;
    in

    let rec output_expression (expr : D_ast.ast_expression) =
        output_identifier   expr.ident;

        match expr.data with
        | Assign            { var; rhs } ->
            output_identifier   var;
            output_expression   rhs
        | DynamicDispatch   { call_on; _method; args } ->
            output_expression   call_on;
            output_identifier   _method;
            output_list         args    output_expression
        | StaticDispatch    { call_on; _method; args } ->
            output_expression call_on;
            output_identifier _method;
            output_list         args    output_expression
        | SelfDispatch      { _method; args } ->
            output_identifier   _method;            
            output_list         args    output_expression
        | If                { predicate; _then; _else } ->
            output_expression   predicate;
            output_expression   _then;
            output_expression   _else
        | While             { predicate; body } -> 
            output_expression   predicate
        | Block             { body } ->
            output_list         body    output_expression
        | New               { _class } ->
            output_identifier   _class
        | IsVoid            { expr } ->
            output_expression   expr
        | BinOp             { left; right; _ } ->
            output_expression   left;
            output_expression   right;
       | UnOp              { expr; _ } ->
            output_expression   expr;
        | Integer i  ->
            output_number       i
        | String str ->
            output_line         str
        | Identifier ident ->
            output_identifier   ident
        | True -> ()
        | False -> ()
        | LetBindingNoInit      { type_ident; variable; _type; _in } ->
            output_identifier   type_ident;
            output_identifier   variable;
            output_identifier   _type;
            output_expression   _in;
        | LetBindingInit        { type_ident; variable; _type; value; _in } ->
            output_identifier   type_ident;
            output_identifier   variable;
            output_identifier   _type;
            output_expression   value;
            output_expression   _in;
        | Case                  { expression; mapping_list } ->
            output_expression   expression;

            let output_case_mapping (mapping : D_ast.ast_case_mapping) =
                output_identifier   mapping.name;
                output_identifier   mapping._type;
                output_expression   mapping.maps_to
            in

            List.iter           output_case_mapping mapping_list
        | Unreachable -> output_line "unreachable"
        | _ -> Printf.printf "Unhandled Expression!\n"; exit 1
    in

    let output_class_map =
        output_line "class_map";
        output_number (List.length ast);

        let output_class (_class : D_ast.ast_class) : unit =
            output_line _class.name.name;
            output_number (List.length _class.attributes);
        
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
                        output_expression init
            in

            List.iter output_attribute _class.attributes
        in

        List.iter output_class ast
    in

    output_class_map
