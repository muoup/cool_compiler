open A_parser
open D_ast

type attribute_data = {
    name : string;
    _type : string;
    init : ast_expression option;
}

type class_data = {
    name : string;
    attributes : attribute_data list;
}

let parse_class_map (data : parser_data) : parser_data * class_data list =
    let parse_attribute (data : parser_data) : (parser_data * attribute_data) =
        let data, attribute_ident = parse_line data in

        match attribute_ident with
        | "no_initializer" ->
            let data, name = parse_line data in
            let data, _type = parse_line data in

            (data, { name = name; _type = _type; init = None; })
        | "initializer" ->
            let data, name = parse_line data in
            let data, _type = parse_line data in
            let data, expr = parse_expression data in

            (data, { name = name; _type = _type; init = Some expr; })
        | x -> raise (Invalid_argument ("Invalid attribute type: " ^ x))
    in

    let parse_class (data : parser_data) : (parser_data * class_data) =
        let data, class_name = parse_line data in
        let data, attributes = parse_list data parse_attribute in

        (data, { name = class_name; attributes = attributes; })
    in

    let data, first_line = parse_line data in

    if first_line <> "class_map" then
        raise (Invalid_argument ("Expected class_map, got " ^ first_line))
    ;

    parse_list data parse_class