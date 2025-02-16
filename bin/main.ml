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


let verify_classes (ast : Ast.ast) : unit =
    let main_flag = ref false in
        let rec check_for_main = function
        | [] -> if not !main_flag then (
            Util.print_error 0 "class Main not found";
        )
        | { Ast.name; _ } :: rest ->
        if name.name = "Main" then
            main_flag := true
        else
            check_for_main rest
    in 
 
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
    find_duplicate ast;
    check_for_main ast


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
        verify_classes ast;
    )
