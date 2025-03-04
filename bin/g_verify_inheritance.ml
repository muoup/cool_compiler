open E_ast_data
open D_ast
open A_util

module StringSet = Set.Make(String)

type inheritance_data = {
    classes : class_map;
    inherited_methods : ast_method StringMap.t;
    inherited_attributes : StringSet.t;
}

let verify_type (classes : class_map) (type_ : ast_identifier) =
    if (type_.name <> "SELF_TYPE") && (not (StringMap.mem type_.name classes)) then
        error_and_exit type_.line_number ("Unknown type name: " ^ type_.name)

let verify_method (data : inheritance_data) (inherited : ast_method) (override : ast_method) : unit =
    let verify_return_type (inherited : ast_identifier) (override : ast_identifier) : unit =
        if not (inherited.name = override.name) then 
            error_and_exit override.line_number "Return type of overrided method differs from its original implementation"
    in

    let rec verify_args (line_number : int) (inherited : ast_param list) (override : ast_param list) : unit =
        match inherited, override with
        | [], [] -> ()
        | inherited_param :: xs, overriden_param :: ys ->
            verify_type data.classes overriden_param._type;

            if not (inherited_param._type.name = overriden_param._type.name) then
                error_and_exit overriden_param._type.line_number "Overriden method param type differs from it's inherited version's"
            ;

            verify_args overriden_param.name.line_number xs ys
        | _, _ -> error_and_exit line_number "Parameter count mismatch!"

    in

    verify_return_type inherited._type override._type;
    verify_args override.name.line_number inherited.params override.params

let rec verify_inherited_methods (data : inheritance_data) (class_name : string) : unit =
    let fold_data (data : inheritance_data) (_method : ast_method) : inheritance_data =
        verify_type data.classes _method._type;
        List.iter (fun (arg : ast_param) -> verify_type data.classes arg._type) _method.params;

        let inherited_method = StringMap.find_opt _method.name.name data.inherited_methods in                

        Option.iter (fun inherited -> verify_method data inherited _method) inherited_method;

        { data with inherited_methods = StringMap.add _method.name.name _method data.inherited_methods }
    in

    let class_data = (StringMap.find class_name data.classes) in
    let new_data = List.fold_left (fold_data) data class_data.class_ref.methods in

    List.iter (fun (sub_class : ast_identifier) -> verify_inherited_methods new_data sub_class.name) class_data.sub_classes

let rec verify_inherited_attributes (data : inheritance_data) (class_name : string) : unit =
    let fold_data (data : inheritance_data) (_attribute : ast_attribute) : inheritance_data =
        let name = match _attribute with
        | AttributeNoInit { name; _type } -> 
                verify_type data.classes _type;
                name
        | AttributeInit   { name; _type; _ } ->
                verify_type data.classes _type;
                name
        in

        if Option.is_some (StringSet.find_opt name.name data.inherited_attributes) then
            error_and_exit name.line_number "Attempt to override an inherited attribute";
        
        { data with inherited_attributes = StringSet.add name.name data.inherited_attributes }
    in

    let class_ = StringMap.find class_name data.classes in
    let new_data = List.fold_left (fold_data) data class_.class_ref.attributes in

    List.iter (fun (sub_class : ast_identifier) -> verify_inherited_attributes new_data sub_class.name) class_.sub_classes

<<<<<<< HEAD
    
let verify_inheritance (data : ast_data) : unit =
    let object_inherited_methods = StringMap.empty in
    let verify_data = { classes = data.classes; inherited_methods = object_inherited_methods; inherited_attributes = StringSet.empty } in
 
    verify_inherited_methods verify_data "Object";
    verify_inherited_attributes verify_data "Object";
