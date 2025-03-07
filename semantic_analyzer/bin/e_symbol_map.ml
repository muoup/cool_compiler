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

let has_symbol (var_name : string) (map : symbol_map) : bool =
    Hashtbl.mem map.data var_name