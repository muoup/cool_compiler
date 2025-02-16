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


let check_duplicate_classes (ast : Ast.ast) : unit =
    let seen = Hashtbl.create (List.length ast) in
    let rec find_duplicate = function
        | [] -> ()
        | { Ast.name; _ } :: rest ->
        if Hashtbl.mem seen name.name then (
            Util.print_error name.line_number ("Class " ^ name.name ^ " redefined.");
            exit 0
        ) else (
            Hashtbl.add seen name.name ();
            find_duplicate rest
        )
    in  
    find_duplicate ast

let () = 
    let rec _print_contents arr = 
        match arr with
        | [] -> ()
        | x :: xs -> 
                Printf.printf "%s\n" x; 
                _print_contents xs
    in
    
    let file_contents = get_file_contents Sys.argv.(1) in
    let ast = Ast.parse_ast file_contents in
    (
        check_duplicate_classes ast
    )
