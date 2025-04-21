open A_parser
open A_builtins
open B_ast
open B_class_map
open B_impl_map
open B_parent_map
open C_parser_data
open D_tac_data
open F_tac_gen
open G_metadata_output
open G_tac_to_cfg
open H_asm_data
open H_asm_gen
open H_dead_code_elim

let change_file_extension (path : string) (new_extension : string) : string =
    Filename.remove_extension path ^ new_extension

let rec output_builtins file_handle builtins_in =
    try 
        let line = input_line builtins_in in
        Printf.fprintf file_handle "%s\n" line;
        output_builtins file_handle builtins_in
    with End_of_file ->
        Printf.fprintf file_handle "\n"

let () = 
    if Array.length Sys.argv < 2 then
        raise (Invalid_argument "Please provide a file name as an argument")
    ;

    let file_name = Sys.argv.(1) in
    let data : parser_data = { line_number = 0; file_handle = open_in file_name } in

    let data, class_map = parse_class_map data in
    let data, impl_map = parse_implementation_map data in
    let data, parent_map = parse_parent_map data in
    let data, ast = parse_ast data in

    let parsed_data : parsed_data = { 
        ast = ast; class_map = class_map; impl_map = impl_map; parent_map = parent_map; 
    } in

    let program_data = organize_parser_data parsed_data in
    let method_tacs = generate_tac program_data in
    let cfg = build_cfg method_tacs in
    let cfg = eliminate_dead_code cfg in
    print_cfg cfg;
    let updated_method_tacs = cfg_to_method_tac_list cfg in


    (* let output_handle = open_out @@ change_file_extension file_name ".cl-tac_us" in
    List.iter (print_tac_cmd (Printf.fprintf output_handle "%s\n")) updated_method_tacs;
    close_out output_handle; *)

    
    let asm = List.map (generate_asm) updated_method_tacs in
    let assembly_handle = open_out (change_file_extension file_name ".s") in
    let output = Printf.fprintf assembly_handle "%s" in

    output builtin_asm;
    output "\n";

    List.iter (fun (asm_ : asm_method) -> print_asm_method asm_ (output)) asm;
    output "\n";

    emit_metadata output program_data;
    output "\n";
    
    close_out assembly_handle
