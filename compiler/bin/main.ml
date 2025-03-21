open A_parser
open B_ast
open B_class_map
open B_impl_map
open B_parent_map
open C_parser_data
open D_tac_data
open F_tac_gen
open G_tac_to_cfg
open H_asm_data
open H_asm_gen

let change_file_extension (path : string) (new_extension : string) : string =
    Filename.remove_extension path ^ new_extension

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

    let parsed_data : parsed_data = { ast = ast; class_map = class_map; impl_map = impl_map; parent_map = parent_map; } in

    let method_tacs = generate_tac parsed_data in

    let asm = List.map (generate_asm) method_tacs in

    (* For now, don't include internal functions (i.e. ones with only a FRAME intialization) *)
    let non_internals = List.filter (fun (asm_ : asm_method) -> List.length asm_.commands <> 1) asm in

    let assembly_handle = open_out (change_file_extension file_name ".s") in
    let output = Printf.fprintf assembly_handle "%s" in

    let runtime = open_in_bin "builtins.s" in
    let s = really_input_string runtime (in_channel_length runtime) in
    output s;
    output "\n";

    List.iter (fun (asm_ : asm_method) -> print_asm_method asm_ (output)) non_internals;
    close_out assembly_handle
