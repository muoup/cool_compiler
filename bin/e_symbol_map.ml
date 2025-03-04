(* Define a standard hashtable for symbols *)
type symbol_map = {
    data : (string, string) Hashtbl.t;
    scoped_data : string list list
}

let new_symbol_map () : symbol_map =
    { data = Hashtbl.create 100; scoped_data = [[]] }

let push_scope (map : symbol_map) : symbol_map =
    { scoped_data = [] :: map.scoped_data; data = map.data }

let pop_scope (map : symbol_map) : symbol_map =
    let hd = List.hd map.scoped_data
    and tl = List.tl map.scoped_data in

    List.iter (fun (var_name : string) -> Hashtbl.remove map.data var_name) hd;

    { scoped_data = tl; data = map.data }

let add_symbol (var_name : string) (var_type : string) (map : symbol_map) : symbol_map =
    let hd = List.hd map.scoped_data
    and tl = List.tl map.scoped_data in

    let new_scope = (var_name :: hd) :: tl in
    let new_map = { scoped_data = new_scope; data = map.data } in

    Hashtbl.add map.data var_name var_type;

    new_map

let get_symbol (var_name : string) (map : symbol_map) : string =
    Hashtbl.find map.data var_name

let _basic_sym_map_test () =
  let map = new_symbol_map () in

  let map = push_scope map in
  let map = add_symbol "a" "Int" map in
  let map = add_symbol "b" "String" map in

  let a = get_symbol "a" map in
  let b = get_symbol "b" map in

  assert (a = "Int");

  assert (b = "String");

  let map = push_scope map in
  let map = add_symbol "a" "String" map in

  let a = get_symbol "a" map in
  assert (a = "String");

  let map = pop_scope map in

  let a = get_symbol "a" map in
  assert (a = "Int");

  Printf.printf "Symbol Map Test Passed\n"