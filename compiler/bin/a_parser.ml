exception Ast_error

type ast_identifier = { name : string; line_number: int; }

type parser_data = {
    line_number         : int;
    file_contents       : string list;
}

let pop_data_lines (data : parser_data) (n : int) : parser_data =
    let rec ntail (lst : 'a list) (n : int) =
        match n with
        | 0 -> lst
        | n -> ntail (List.tl lst) (n - 1)
    in

    { line_number = data.line_number + n; file_contents = ntail data.file_contents n }

let parse_int (data : parser_data) : (parser_data * int) =
    match int_of_string_opt (List.hd data.file_contents) with
    | Some x -> (pop_data_lines data 1), x
    | None ->
        Printf.printf "Unknown line number: %s at line %d\n" (List.hd data.file_contents) data.line_number;
        raise Ast_error

let parse_identifier (data : parser_data) : (parser_data * ast_identifier) =
    let data, line_number = parse_int data in
    let identifier = List.hd data.file_contents in

    (pop_data_lines data 1, { name = identifier; line_number = line_number })

let parse_line (data : parser_data) : (parser_data * string) =
    let val_ = List.hd data.file_contents in

    (pop_data_lines data 1, val_)

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