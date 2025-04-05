open A_util
open B_ast
open B_class_map
open B_impl_map
open B_parent_map

type parsed_data = {
    ast     : ast_class list;
    class_map : class_data list;
    impl_map : impl_class list;
    parent_map : parent_relation list;
}

type program_class_data = {
    name  : string;

    attributes  : attribute_data list;
    methods     : impl_method list;
}

type program_data = {
    ast         : ast_class list;

    data_map    : program_class_data StringMap.t;
    overriden_classes : StringSet.t;
}

let organize_parser_data (parsed_data : parsed_data) : program_data =
    let rec aggregate_overrides (parent_relation : parent_relation list) (acc : StringSet.t) : StringSet.t =
        match parent_relation with
        | [] -> acc
        | { child; parent } :: rest ->
            let new_acc = StringSet.add child acc in
            aggregate_overrides rest new_acc
    in

    let rec internal_rec (class_data : class_data list) (impl_class : impl_class list) : program_class_data list =
        match class_data, impl_class with
        | [], [] -> []
        | c :: cs, i :: is ->
            { name = c.name; attributes = c.attributes; methods = i.methods } :: internal_rec cs is
        | _, _ -> raise (Invalid_argument "This shouldnt happen!")
    in

    let overriden_classes = aggregate_overrides parsed_data.parent_map StringSet.empty in

    let pcd_list = internal_rec parsed_data.class_map parsed_data.impl_map in
    let pcd_map = List.fold_left (
        fun acc pcd -> StringMap.add pcd.name pcd acc 
    ) StringMap.empty pcd_list in

    { ast = parsed_data.ast; data_map = pcd_map; overriden_classes = overriden_classes }

let get_dispatch (data : program_data) (class_name : string) (method_name : string) : int =
    let class_data = StringMap.find class_name data.data_map in
    let method_id = find_index class_data.methods (fun impl_method -> impl_method.name = method_name) in

    method_id