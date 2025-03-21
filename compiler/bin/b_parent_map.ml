open A_parser

type parent_relation = {
    parent: string;
    child: string;
}

let parse_parent_map (data : parser_data) : (parser_data * parent_relation list) =
    let parse_parent_relation (data : parser_data) : (parser_data * parent_relation) =
        let data, parent = parse_line data in
        let data, child = parse_line data in

        data, { parent = parent; child = child; }
    in

    let data, first_line = parse_line data in

    if first_line <> "parent_map" then
        raise (Invalid_argument ("Expected parent_map, got " ^ first_line))
    ;

    parse_list data parse_parent_relation