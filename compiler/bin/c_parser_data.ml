open A_format
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
    class_name  : string;

    class_data  : class_data;
    impl_class  : impl_class
}

type program_data = {
    ast         : ast_class list;

    data_map    : program_class_data StringMap.t
}

let organize_parser_data (parsed_data : parsed_data) : program_data =
    let rec internal_rec (class_data : class_data list) (impl_class : impl_class list) : program_class_data list =
        match class_data, impl_class with
        | [], [] -> []
        | c :: cs, i :: is ->
            { class_name = c.name; class_data = c; impl_class = i } :: internal_rec cs is
        | _, _ -> raise (Invalid_argument "This shouldnt happen!")
    in

    let pcd_list = internal_rec parsed_data.class_map parsed_data.impl_map in
    let pcd_map = List.fold_left (
        fun acc pcd -> StringMap.add pcd.class_data.name pcd acc 
    ) StringMap.empty pcd_list in

    { ast = parsed_data.ast; data_map = pcd_map }

let get_dispatch (data : parsed_data) (class_name : string) (method_name : string) : string =
    let class_ref = List.find (fun (cls : impl_class) -> cls.name = class_name) data.impl_map in
    let method_ref = List.find (fun (fn : impl_method) -> fn.name = method_name) class_ref.methods in

    method_ref.name