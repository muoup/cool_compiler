open D_ast
open A_util

module StringMap = Map.Make(String)

type class_data = {
    class_ref   : ast_class;

    attributes  : ast_attribute  StringMap.t;
    methods     : ast_method     StringMap.t;
    sub_classes : ast_identifier list;
}
type class_map = class_data StringMap.t

type ast_data = {
    classes : class_map
}

let supertype_of (classes : class_map) (class_name : ast_identifier) : ast_identifier =
    let instance = (StringMap.find class_name.name classes).class_ref.inherits in

    match instance with
    | None -> (StringMap.find "Object" classes).class_ref.name
    | Some class_ -> class_

let rec is_subtype_of (classes : class_map) (lhs : ast_identifier) (rhs : ast_identifier) : bool =
    match lhs.name, rhs.name with
    | x, y        when x = y  -> true
    | "Object", _             -> false
    | _, _                    -> is_subtype_of classes (supertype_of classes lhs) rhs

let generate_ast_data (ast : ast) : ast_data =
    let map_fold (classes : class_map) (_class : ast_class) =
        let method_fold (map : ast_method StringMap.t) (method_ : ast_method) =
            if StringMap.mem (method_.name.name) map then
                error_and_exit method_.name.line_number "Duplicate method definition"
            ;

            StringMap.add method_.name.name method_ map
        in

        let attribute_fold (map : ast_attribute StringMap.t) (attribute : ast_attribute) =
            let name = match attribute with
            | AttributeNoInit   { name; _ } -> name
            | AttributeInit     { name; _ } -> name
            in

            if StringMap.mem name.name map then
                error_and_exit name.line_number "Duplicate attribute definition" 
            ;

            StringMap.add name.name attribute map
        in

        let new_data = {
            class_ref = _class;
            
            methods = List.fold_left method_fold StringMap.empty _class.methods;
            attributes = List.fold_left attribute_fold StringMap.empty _class.attributes;
            sub_classes = [];
        }
        in

        StringMap.add _class.name.name new_data classes
    in

    let populate_data (classes : class_map) (class_ : ast_class) : class_map =
        if class_.name.name <> "Object" then
            let supertype = (supertype_of classes class_.name) in
            StringMap.update supertype.name (Option.map (function _data -> { _data with sub_classes = (class_.name :: _data.sub_classes) })) classes
        else
            classes
    in

    let class_map = List.fold_left (map_fold) StringMap.empty ast in
    let class_map = List.fold_left (populate_data) class_map ast in

    { classes = class_map }
