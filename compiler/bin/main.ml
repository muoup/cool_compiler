open A_parser
open D_ast
open D_class_map
open D_impl_map
open D_parent_map
open E_parser_data
open G_tac_data
open J_tac_gen

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

let change_file_extension (path : string) (new_extension : string) : string =
    Filename.remove_extension path ^ new_extension

let () = 
    if Array.length Sys.argv < 2 then
        raise (Invalid_argument "Please provide a file name as an argument")
    ;

    let file_name = Sys.argv.(1) in

    let file_contents = get_file_contents file_name in
    let data : parser_data = { line_number = 0; file_contents = file_contents; } in

    let data, class_map = parse_class_map data in
    let data, impl_map = parse_implementation_map data in
    let data, parent_map = parse_parent_map data in
    let data, ast = parse_ast data in

    let parsed_data : parsed_data = { ast = ast; class_map = class_map; impl_map = impl_map; parent_map = parent_map; } in

    let tac_cmds = generate_tac parsed_data in

    let output_handle = open_out @@ change_file_extension file_name ".cl-tac" in
    List.iter (print_tac_cmd (Printf.fprintf output_handle "%s\n")) tac_cmds;
    close_out output_handle;

    ()
