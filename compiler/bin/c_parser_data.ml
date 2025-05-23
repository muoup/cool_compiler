open A_util
open B_ast
open B_class_map
open B_impl_map
open B_parent_map

type parsed_data = {
    ast         : ast_class list;
    class_map   : class_data list;
    impl_map    : impl_class list;
    parent_map  : parent_map;
}

type program_class_data = {
    name  : string;

    attributes  : attribute_data list;
    methods     : impl_method list;
}

type program_data = {
    ast         : ast_class list;

    parent_map  : parent_map;
    data_map    : program_class_data StringMap.t;
    overriden_classes : StringSet.t;
}

let organize_parser_data (parsed_data : parsed_data) : program_data =
    let rec internal_rec (class_data : class_data list) (impl_class : impl_class list) : program_class_data list =
        match class_data, impl_class with
        | [], [] -> []
        | c :: cs, i :: is ->
            { name = c.name; attributes = c.attributes; methods = i.methods } :: internal_rec cs is
        | _, _ -> raise (Invalid_argument "This shouldnt happen!")
    in

    let overriden_classes = StringMap.fold (
        fun child parent acc -> StringSet.add parent acc
    ) parsed_data.parent_map StringSet.empty in

    let pcd_list = internal_rec parsed_data.class_map parsed_data.impl_map in
    let pcd_map = List.fold_left (
        fun acc pcd -> StringMap.add pcd.name pcd acc 
    ) StringMap.empty pcd_list in

    { 
        ast = parsed_data.ast; 
        data_map = pcd_map; 
        overriden_classes = overriden_classes;
        parent_map = parsed_data.parent_map;
    }

let get_dispatch (data : program_data) (class_name : string) (method_name : string) : int =
    let class_data = StringMap.find class_name data.data_map in
    let method_id = find_index class_data.methods (fun impl_method -> impl_method.name = method_name) in

    (* new.### is 0 so add 1 *)
    method_id + 1

let parent_of (data : program_data) (class_name : string) : string =
    match StringMap.find_opt class_name data.parent_map with
    | Some parent -> parent
    | None -> 
        Printf.printf "Warning: Class %s has no parent.\n" class_name;
        StringMap.iter (fun k v -> Printf.printf "Class: %s, Parent: %s\n" k v) data.parent_map;
        exit 0

let get_internal_signature (method_name : string) : string * string list =
    match method_name with
    | "out_string" -> "SELF_TYPE", ["String"]
    | "out_int" -> "SELF_TYPE", ["Int"]
    | "in_string" -> "String", []
    | "in_int" -> "Int", []
    | "length" -> "Int", []
    | "concat" -> "String", ["String"]
    | "substr" -> "String", ["Int"; "Int"]
    | "abort" -> "Object", []
    | "type_name" -> "String", []
    | "copy" -> "SELF_TYPE", []
    | _ ->
        Printf.printf "Warning: Method %s not found in internal signature.\n" method_name;
        exit 0

let get_internal_name (method_name : string) : string =
    match method_name with
    | "out_string" -> "IO.out_string"
    | "out_int" -> "IO.out_int"
    | "in_string" -> "IO.in_string"
    | "in_int" -> "IO.in_int"
    | "length" -> "String.length"
    | "concat" -> "String.concat"
    | "substr" -> "String.substr"
    | "abort" -> "Object.abort"
    | "type_name" -> "Object.type_name"
    | "copy" -> "Object.copy"
    | _ ->
        Printf.printf "Warning: Method %s not found in internal signature.\n" method_name;
        exit 0

let rec get_subtypes (data : program_data) (_type : string) : string list = 
    StringMap.bindings data.parent_map
    |>  List.filter_map (
            fun (child, parent) ->
                if parent = _type then Some child else None
        )

(* Order - distance from Object in hierarchy *)
let rec type_order (data : program_data) (_type : string) : int =
    match StringMap.find_opt _type data.parent_map with
    | Some parent ->
        1 + type_order data parent
    | None -> 0

let rec get_method_name (data : program_data) (class_name : string) (method_name : string) : string =
    let _method =
        List.find_opt (fun (class_data : ast_class) -> class_data.name.name = class_name) data.ast
        |>  Option.map (fun (class_data : ast_class) ->
                List.find_opt (
                    fun (method_data : ast_method) -> method_data.name.name = method_name
                ) class_data.methods
            )
        |>  Option.join 
    in 

    match _method with
    | Some _ ->
        method_name_gen class_name method_name
    | None ->
        if class_name = "Object" then
            get_internal_name method_name
        else
            let parent = parent_of data class_name in
            get_method_name data parent method_name

let rec get_method_signature (data : program_data) (class_name : string) (method_name : string) : string * string list =
    let _method =
        List.find_opt (fun (class_data : ast_class) -> class_data.name.name = class_name) data.ast
        |>  Option.map (fun (class_data : ast_class) ->
                List.find_opt (
                    fun (method_data : ast_method) -> method_data.name.name = method_name
                ) class_data.methods
            )
        |>  Option.join 
    in
    
    match _method with
    | Some method_data ->
        let params = List.map (fun (param : ast_param) -> param._type.name) method_data.params in
        method_data._type.name, params
    | None ->
        if class_name = "Object" then
            get_internal_signature method_name
        else
            let parent = parent_of data class_name in
            get_method_signature data parent method_name