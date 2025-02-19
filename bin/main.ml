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
    
    let handle = open_in file_dir in
    let contents = generate_array handle [] in

    close_in handle;
    contents

let () = 
    (* Code courtesy of stack overflow user ivg's answer to question id: 70978234 *)
    let change_file_extension (path : string) (new_extension : string) : string =
        Filename.remove_extension path ^ "." ^ new_extension
    in

    let rec print_contents arr = 
        match arr with
        | [] -> ()
        | x :: xs -> 
                Printf.printf "%s\n" x; 
                print_contents xs
    in
    
    let file_name = Sys.argv.(1) in
    let file_contents = get_file_contents file_name in
    let ast = Ast.parse_ast file_contents in (
    Verify_classes.verify_classes ast;
    Output.output_ast ast (change_file_extension file_name "cl-type")
    )
