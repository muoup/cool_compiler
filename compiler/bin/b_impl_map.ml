open A_parser
open B_ast

type impl_method ={
    name: string;
    formals: string list;
    parent: string;
    body: ast_expression;
}

type impl_class = {
    name : string;
    methods : impl_method list;
}

let parse_implementation_map (data : parser_data) : (parser_data * impl_class list) =
    let parse_impl_class (data : parser_data) : (parser_data * impl_class) =
        let parse_impl_method (data : parser_data) : (parser_data * impl_method) =
            let data, method_name = parse_line data in
            let data, formals = parse_list data parse_line in
            let data, parent = parse_line data in
            let data, body = parse_expression data in

            data, { name = method_name; formals = formals; parent = parent; body = body; }
        in

        let data, class_name = parse_line data in

        let data, methods = parse_list data parse_impl_method in

        data, { name = class_name; methods = methods; }
    in

    let data, first_line = parse_line data in

    if first_line <> "implementation_map" then
        raise (Invalid_argument ("Expected implementation_map, got " ^ first_line));

    parse_list data parse_impl_class