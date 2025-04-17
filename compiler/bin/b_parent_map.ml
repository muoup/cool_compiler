open A_parser
open A_util

type parent_map = string StringMap.t

let parse_parent_map (data : parser_data) : (parser_data * parent_map) =
    let parse_parent_relation (data : parser_data) : (parser_data * (string * string)) =
        let data, parent = parse_line data in
        let data, child = parse_line data in

        data, (parent, child)
    in

    let data, first_line = parse_line data in

    if first_line <> "parent_map" then
        raise (Invalid_argument ("Expected parent_map, got " ^ first_line))
    ;

    let data, relations = parse_list data parse_parent_relation in
    data, StringMap.of_seq (List.to_seq relations)