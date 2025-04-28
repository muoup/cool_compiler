open A_parser
open A_builtins
open B_ast
open B_class_map
open B_impl_map
open B_parent_map
open C_parser_data
open D_tac_data
open E_tac_parsing
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

    let tac_commands = parse_tac_file data in
    let method_tacs = cmds_to_method tac_commands in

    let cfg = build_cfg method_tacs in
    let cfg = eliminate_dead_code cfg in
    (* print_cfg cfg; *)
    let optimized_method_tacs = cfg_to_method_tac_list cfg in

    (* ONLY FOR PA4C1 *)
    let main_dot_main = get_main_main optimized_method_tacs in
    let optimized_tac_commands = all_relevant_tac_commands main_dot_main in
    let output_handle = open_out @@ change_file_extension file_name ".cl-tac" in
    List.iter (print_tac_cmd_for_pa4c1 (Printf.fprintf output_handle "%s\n")) optimized_tac_commands;
    close_out output_handle;