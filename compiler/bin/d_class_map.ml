open A_parser
open D_ast

type class_data = {
    name : string;
    attributes : ast_attribute list;
}

let parse_class_map (data : parser_data) : parser_data * class_data list =
    let parse_attribute (data : parser_data) : (parser_data * ast_attribute) =
        let data, _type = parse_line data in

        match _type with
        | "no_initializer" ->
            let data, name = parse_identifier data in
            let data, _type = parse_identifier data in

            (data, AttributeNoInit { name = name; _type = _type; })
        | "initializer" ->
            let data, name = parse_identifier data in
            let data, _type = parse_identifier data in
            let data, expr = parse_expression data in

            (data, AttributeInit { name = name; _type = _type; init = expr; })
    in

    let parse_class (data : parser_data) : (parser_data * class_data) =
        let data, class_name = parse_line data in
        let data, attributes = parse_list data parse_attribute in

        (data, { name = class_name; attributes = attributes; })
    in

    let data, first_line = parse_line data in

    if first_line <> "class_map" then
        Printf.printf "Error: Expected class_map, got %s\n" first_line;
        exit 1
    ;

    let data, classes = parse_list data parse_class in
    
    data, List.map
      (fun class_ -> { name = class_.name; attributes = class_.attributes; })
      classes