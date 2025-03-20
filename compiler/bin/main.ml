open A_parser
open D_ast
open D_class_map
open D_impl_map
open D_parent_map
open E_parser_data
open G_tac_data
open J_tac_gen

let change_file_extension (path : string) (new_extension : string) : string =
    Filename.remove_extension path ^ new_extension

let () = 
    if Array.length Sys.argv < 2 then
        raise (Invalid_argument "Please provide a file name as an argument")
    ;

    let file_name = Sys.argv.(1) in
    let data : parser_data = { line_number = 0; file_handle = open_in file_name } in

    (* let output_handle = open_out @@ change_file_extension file_name ".cl-tac" in
    Printf.fprintf output_handle "label Main_main_0\n";
    Printf.fprintf output_handle "t$0 <- int 0\n";
    Printf.fprintf output_handle "return t$0\n";
    close_out output_handle; *)

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
