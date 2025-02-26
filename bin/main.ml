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
    | e -> Printf.printf "Cannot open file: %s\n" file_dir; exit 1

let () = 
    (* Code courtesy of stack overflow user ivg's answer to question id: 70978234 *)
    let change_file_extension (path : string) (new_extension : string) : string =
        Filename.remove_extension path ^ "." ^ new_extension
    in

    let rec _print_contents arr = 
        match arr with
        | [] -> ()
        | x :: xs -> 
                Printf.printf "%s\n" x; 
                _print_contents xs
    in
    
    let file_name = Sys.argv.(1) in
    let file_contents = get_file_contents file_name in
    let ast = D_ast.parse_ast file_contents in 
    let ast_data = E_ast_data.generate_ast_data ast in

    (*
    StringMap.iter (fun key data ->
        Printf.printf "Class Name: %s\n" key;
        Printf.printf "Subclasses: ";
        List.iter (function (sub_class : D_ast.ast_identifier) -> Printf.printf "%s " sub_class.name) data.sub_classes;
        Printf.printf "\n";
    ) ast_data.classes;
    *)

    G_verify_classes.verify_classes ast;
    G_verify_inheritance.verify_inheritance ast_data;
    let _ = G_typecheck.verify_ast ast in
    F_output.output_ast ast_data (change_file_extension file_name "cl-type")
