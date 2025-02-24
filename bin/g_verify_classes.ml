open D_ast
open A_util

module StringMap = Map.Make(String)

let verify_classes (ast : ast) : unit =
  let main_flag = ref false in
      let rec check_for_main = function
      | [] -> if not !main_flag then (
          error_and_exit 0 "class Main not found";
      )
      | { name; _ } :: rest ->
      if name.name = "Main" then
          main_flag := true
      else
          check_for_main rest
  in 

  let rec illegal_inheritance = function
  | [] -> ()
  | { name; inherits; _ } :: rest ->
    if (Option.is_some inherits) then
        if (Option.get inherits).name = "Bool" || (Option.get inherits).name = "Int" || (Option.get inherits).name = "String" then (
            error_and_exit (Option.get inherits).line_number ("class " ^ name.name ^ " inherits from " ^ (Option.get inherits).name)
        ) else ()
    else
        illegal_inheritance rest
  in

  let extract_inherited_names (ast : ast) : string list =
    List.filter_map (fun cls -> 
      match cls.inherits with
      | Some id -> Some id.name
      | None -> None
    ) ast
  in
  let rec check_inherits = function
  | [] ->  ()
  | { name; inherits; _ } :: rest ->
  if (Option.is_some inherits) then (
  if not (List.mem name.name (extract_inherited_names ast) || (Option.get inherits).name = "IO") then
    error_and_exit name.line_number ("class " ^ name.name ^ " inherits from unknown class " ^ (Option.get inherits).name);
    check_inherits rest    
  )
  in

  let seen = Hashtbl.create (List.length ast) in
  let rec find_duplicates = function
      | [] -> ()
      | { name; _ } :: rest ->
      if Hashtbl.mem seen name.name then (
          error_and_exit name.line_number ("Class " ^ name.name ^ " redefined.")
      ) else (
          Hashtbl.add seen name.name ();
          find_duplicates rest
      )
  in  

  (* This is slightly ugly, but it's because I copied my PA1 code and worked around it *)
  let convert_ast_class (ast_class : ast_class) : (string * string list) =
    let str = ast_class.name.name in
    let str_list = match ast_class.inherits with
      | Some inherits -> [inherits.name]
      | None -> []
    in
    (str, str_list)
  in
  let convert_ast_class_list (ast_list : ast_class list) : (string * string list) list =
    List.map convert_ast_class ast_list @ [("IO", [])]
  in 
  let check_cycles lst =
    let remove_key key lst = List.map (fun (k, values) -> (k, List.filter ((<>) key) values)) lst in
    let rec process acc remaining =
      match List.find_opt (fun (_, values) -> values = []) remaining with
      | Some (key, _) ->
          let updated_rest = remove_key key (List.remove_assoc key remaining) in
          process (key :: acc) updated_rest
      | None -> if remaining = [] then () else (
        error_and_exit 0 ("inheritance cycle: "  ^ (String.concat " " (List.map fst remaining))) 
      )
      in process [] lst
  in

  check_for_main ast;
  illegal_inheritance ast;
  check_inherits ast;
  find_duplicates ast;
  check_cycles (convert_ast_class_list ast)
