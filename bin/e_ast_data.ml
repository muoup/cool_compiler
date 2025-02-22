open D_ast

type class_data = {
    class_ref   : ast_class;
    sub_classes : string list;
}

module StringMap = Map.Make(String)
type class_map = class_data StringMap.t

type ast_data = {
    classes : class_map
}

let supertype_of (classes : class_map) (class_name : string) : string =
    let instance = (StringMap.find class_name classes).class_ref.inherits in

    match instance with
    | None -> "Object"
    | Some class_ -> class_.name

let rec is_subtype_of (classes : class_map) (lhs : string) (rhs : string) : bool =
    match lhs, rhs with
    | x, y        when x = y  -> true
    | x, "Object"             -> false
    | x, y                    -> is_subtype_of classes lhs (supertype_of classes rhs) 

let generate_ast_data (ast : ast) : class_map =
    let map_fold (classes : class_map) (_class : ast_class) =
        let new_data = {
            class_ref = _class;
            sub_classes = [];
        }
        in

        StringMap.add _class.name.name new_data classes
    in

    let populate_data (classes : class_map) (class_ : ast_class) : class_map =
        let data = StringMap.find class_.name.name classes in

        match data.class_ref.inherits with
        | None          -> classes
        | Some parent   -> StringMap.update parent.name (Option.map (fun _data -> { _data with sub_classes = (parent.name :: _data.sub_classes) })) classes
    in

    let class_map = List.fold_left (map_fold) StringMap.empty ast in
    let class_map = List.fold_left (populate_data) class_map ast in

    class_map
