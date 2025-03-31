open A_format
open B_class_map
open B_impl_map
open C_parser_data

type class_metadata = {
    class_name : string;

    method_table : string list;
    attribute_table : string list;
}

type metadata = {
    class_metadata : class_metadata StringMap.t;  
}

let generate_class_metadata (pcd : program_class_data) : class_metadata =
    let method_table = List.map (
        fun (impl_method : impl_method) -> impl_method.name
    ) pcd.impl_class.methods in
    let attribute_table = List.map (
        fun (attr_data : attribute_data) -> attr_data.name
    ) pcd.class_data.attributes in

    { class_name = pcd.class_name; method_table; attribute_table }

let generate_metadata (parsed_data : parsed_data) : metadata =
    let program_data = organize_parser_data parsed_data in
    let metadata = StringMap.map (generate_class_metadata) program_data.data_map in

    { class_metadata = metadata }
