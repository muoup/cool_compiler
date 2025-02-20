let output_ast (ast : Ast.ast) (file_path : string) : unit =
    let oc = open_out file_path in
    let output_line = fun (line : string) -> Printf.fprintf oc "%s\n" line in
    let output_number = fun (num : int) -> Printf.fprintf oc "%d\n" num in

    let output_class_map =
        output_line "class_map";
        output_number (List.length ast);

        let output_class (_class : Ast.ast_class) : unit =
            output_line _class.name.name;
            output_number (List.length _class.attributes);
        
            let output_attribute (_attr : Ast.ast_attribute) : unit =
                match _attr with
                | AttributeNoInit   { name; _type } ->
                        output_line "no_initializer";
                        output_line name.name;
                        output_line _type.name
                | AttributeInit     { name; _type; init = _ } ->
                        output_line "initializer";
                        output_line name.name;
                        output_line _type.name;
            in

            List.iter output_attribute _class.attributes
        in

        List.iter output_class ast
    in

    output_class_map
