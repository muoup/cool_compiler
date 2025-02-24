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

let rec get_attributes (classes : class_map) (class_name : ast_identifier) : ast_attribute list =
    let _class = StringMap.find class_name.name classes in
    let self_attributes = _class.class_ref.attributes in

    match _class.class_ref.inherits with
    | None -> self_attributes
    | Some inherit_from -> (get_attributes classes inherit_from) @ self_attributes

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
        let method_fold (map : ast_method StringMap.t) (_method : ast_method) =
            if StringMap.mem (_method.name.name) map then
                error_and_exit _method.name.line_number "Duplicate method definition"
            ;

            let self_type_param_test (param : ast_param) =
                if param._type.name = "SELF_TYPE" then
                    error_and_exit param.name.line_number "SELF_TYPE cannot be used in a method parameter";

                ()
            in

            List.iter (self_type_param_test) _method.params;

            StringMap.add _method.name.name _method map
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
            
            if not (StringMap.mem supertype.name classes) then error_and_exit supertype.line_number "Unknown inherited class";

            StringMap.update supertype.name (Option.map (function _data -> { _data with sub_classes = (class_.name :: _data.sub_classes) })) classes
        else
            classes
    in

    let class_map = List.fold_left (map_fold) StringMap.empty ast in
    let class_map = List.fold_left (populate_data) class_map ast in

    match StringMap.find_opt "Main" class_map with
    | None -> error_and_exit 0 "Main class not defined!"
    | Some class_data ->
            match StringMap.find_opt "main" class_data.methods with
            | None -> error_and_exit 0 "Main class does not have a main method defined!"
            | Some method_ -> if List.length method_.params <> 0 then error_and_exit 0 "Main.main() must be defined with no parameters"
    ;

    { classes = class_map }
