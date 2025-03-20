exception Ast_error

type ast_identifier = { name : string; line_number: int; }

type parser_data = {
    line_number         : int;
    file_handle         : in_channel;
}

let parse_line (data : parser_data) : (parser_data * string) =
    let val_ = input_line data.file_handle in

    { data with line_number = data.line_number + 1 }, val_

let parse_int (data : parser_data) : (parser_data * int) =
    let data, _val = parse_line data in

    match int_of_string_opt _val with
    | Some x -> data, x
    | None ->
        Printf.printf "Unknown integer: %s at line %d\n" _val data.line_number;
        raise Ast_error

let parse_identifier (data : parser_data) : (parser_data * ast_identifier) =
    let data, line_number = parse_int data in
    let data, identifier = parse_line data in

    (data, { name = identifier; line_number = line_number })

let parse_list (data : parser_data) (mapping : parser_data -> (parser_data * 'a)) : (parser_data * 'a list) =
    let rec internal_rec data mapping (i : int) (acc : 'a list) : (parser_data * 'a list) =
        match i with
        | 0 -> (data, acc)
        | i -> 
            let data, produced = mapping data in
            let data, acc = internal_rec data mapping (i - 1) acc in

            (data, produced :: acc)
    in

    let data, line_count = parse_int data in

    internal_rec data mapping line_count [] 