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
    let file_contents = get_file_contents Sys.argv.(1) in
    let ast = Ast.parse_ast file_contents in
    Type_checker.verify_ast ast;
    Printf.printf "Verification Successful!\n"
