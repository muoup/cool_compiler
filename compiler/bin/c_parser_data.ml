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

let get_dispatch (data : parsed_data) (class_name : string) (method_name : string) : string =
    let class_ref = List.find (fun (cls : impl_class) -> cls.name = class_name) data.impl_map in
    let method_ref = List.find (fun (fn : impl_method) -> fn.name = method_name) class_ref.methods in

    method_ref.name