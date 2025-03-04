open D_ast
open A_util

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

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

let rec get_attributes (classes : class_map) (class_name : string) : ast_attribute list =
    let _class = StringMap.find class_name classes in
    let self_attributes = _class.class_ref.attributes in

    match _class.class_ref.inherits with
    | None -> self_attributes
    | Some inherit_from -> (get_attributes classes inherit_from.name) @ self_attributes

let supertype_of (classes : class_map) (class_name : string) : ast_identifier =
    let instance = 
        match StringMap.find_opt class_name classes with
        | None -> 
            raise (Failure ("Class " ^ class_name ^ " not found"))
        | Some class_ -> class_.class_ref.inherits
    in

    match instance with
    | None -> (StringMap.find "Object" classes).class_ref.name
    | Some class_ -> class_

let rec is_subtype_of (classes : class_map) (lhs : string) (rhs : string) : bool =
    match lhs, rhs with
    | x, y        when x = y  -> true
    | "Object", _             -> false
    | _, _                    -> is_subtype_of classes (supertype_of classes lhs).name rhs

let join_classes (current_class : string) (classes : class_map) (lhs : string) (rhs : string) =
    let rec create_lhs_tree (set : StringSet.t) (class_name : string) : StringSet.t =
        let set = StringSet.add class_name set in

        match class_name with
        | "Object" -> set
        | _        -> create_lhs_tree set (supertype_of classes class_name).name
    in


    match lhs, rhs with
    | "SELF_TYPE", "SELF_TYPE" -> "SELF_TYPE"
    | _, _                     ->
        let lhs = if lhs = "SELF_TYPE" then current_class else lhs in
        let rhs = if rhs = "SELF_TYPE" then current_class else rhs in
         
        let lhs_tree = create_lhs_tree StringSet.empty lhs in

        let rec find_common_ancestor (class_name : string) : string =
            if StringSet.mem class_name lhs_tree then class_name
            else find_common_ancestor (supertype_of classes class_name).name
        in

        find_common_ancestor rhs

let get_static_dispatch (classes : class_map) (class_name : string) (method_name : string) : ast_method option =
    let class_data = StringMap.find class_name classes in
    StringMap.find_opt method_name class_data.methods

let rec get_dispatch (classes : class_map) (class_name : string) (method_name : string) : ast_method option =
    let class_data = StringMap.find class_name classes in
    let _method = StringMap.find_opt method_name class_data.methods in

    match _method with
    | Some _method -> Some _method
    | None -> 
        if class_name = "Object" then
            None
        else

        let inherits = match class_data.class_ref.inherits with
        | None -> "Object"
        | Some inherit_from -> inherit_from.name
        in

        get_dispatch classes inherits method_name

let generate_ast_data (ast : ast) : ast_data =
    let map_fold (classes : class_map) (_class : ast_class) =
        let method_fold (map : ast_method StringMap.t) (_method : ast_method) =
            if StringMap.mem (_method.name.name) map then
                error_and_exit _method.name.line_number "Duplicate method definition"
            ;

            let self_type_param_test (param : ast_param) =
                if param._type.name = "SELF_TYPE" then 
                    error_and_exit param.name.line_number "SELF_TYPE cannot be used in a method parameter"; 
                if param.name.name = "self" then 
                    error_and_exit param.name.line_number ("A parameter cannot be named " ^ param.name.name);
                ()
            in

            let has_duplicate_names (params : ast_param list) : bool =
                let rec aux (seen : StringSet.t) (rest : ast_param list) : bool =
                  match rest with
                  | [] -> false
                  | { name = { name = param_name; _ }; _ } :: tail ->
                      if StringSet.mem param_name seen then true
                      else aux (StringSet.add param_name seen) tail
                in
                aux StringSet.empty params
            in

            if (has_duplicate_names _method.params) then (
                error_and_exit _method.name.line_number ("Duplicate parameters in method " ^ _method.name.name);
            );

            List.iter (self_type_param_test) _method.params;

            StringMap.add _method.name.name _method map
        in

        let attribute_fold (map : ast_attribute StringMap.t) (attribute : ast_attribute) =
            let name = match attribute with
            | AttributeNoInit   { name; _ } -> name
            | AttributeInit     { name; _ } -> name
            in

            if StringMap.mem name.name map then
                error_and_exit name.line_number "Duplicate attribute definition";

            if (name.name = "self") then
                error_and_exit name.line_number "An attribute cannot be named self";
        

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
            let supertype = (supertype_of classes class_.name.name) in
            
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
