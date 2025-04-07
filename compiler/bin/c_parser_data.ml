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

    method_id

let parent_of (data : program_data) (class_name : string) : string =
    match StringMap.find_opt class_name data.parent_map with
    | Some parent -> parent
    | None -> 
        Printf.printf "Warning: Class %s has no parent.\n" class_name;
        StringMap.iter (fun k v -> Printf.printf "Class: %s, Parent: %s\n" k v) data.parent_map;
        exit 0

let get_method_signature (data : program_data) (class_name : string) (method_name : string) : string * (string * string) list =
    let rec recursive_routine (class_name : string) (method_name : string) =
        let class_data = List.find (fun (class_data : ast_class) -> class_data.name.name = class_name) data.ast in
        
        let _method = List.find_opt (
            fun (method_data : ast_method) -> method_data.name.name = method_name
        ) class_data.methods in

        match _method with
        | Some method_data -> 
            method_data._type.name, List.map (fun (param : ast_param) -> param.name.name, param._type.name) method_data.params
        | None ->

            if class_name = "Object" then
                raise (Invalid_argument ("Method not found: " ^ method_name))
            else
                let parent = parent_of data class_name in
                recursive_routine parent method_name
    in
    
    let return_type, params = recursive_routine class_name method_name in
    let return_type = if return_type = "SELF_TYPE" then class_name else return_type in

    return_type, params