open A_parser
open D_ast
open D_class_map
open D_impl_map
open D_parent_map

(* File IO *)
let get_file_contents file_dir : string list =
    let rec generate_array file_handle arr =
        try
            let line = input_line file_handle in
            flush stdout;

            generate_array file_handle (arr @ [line])
        with 
        | End_of_file -> arr
        | e -> raise e
    in
 
    try
        let handle = open_in file_dir in
        let contents = generate_array handle [] in

        close_in handle;
        contents
    with
    | _ -> raise (Invalid_argument "File not found")

let () = 
    (* Code courtesy of stack overflow user ivg's answer to question id: 70978234 *)
    let change_file_extension (path : string) (new_extension : string) : string =
        Filename.remove_extension path ^ "." ^ new_extension
    in

    if Array.length Sys.argv < 2 then
        raise (Invalid_argument "Please provide a file name as an argument")
    ;

    let file_name = Sys.argv.(1) in
    let file_contents = get_file_contents file_name in

    let data : parser_data = { line_number = 0; file_contents = file_contents; } in
    
    Printf.printf "Parsing class map\n";
    let data, class_map = parse_class_map data in

    Printf.printf "Parsing implementation map\n";
    let data, impl_map = parse_implementation_map data in

    Printf.printf "Parsing parent map\n";
    let data, parent_map = parse_parent_map data in

    Printf.printf "Parsing AST\n";
    let ast = parse_ast data in
    ()