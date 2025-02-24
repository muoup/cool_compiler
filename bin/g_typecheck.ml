open D_ast
open A_util
module AstIdentifierSet = Set.Make(String)
(* At some point the environment will have to be passed through these functions (class, method, objects) *)
type class_environment = {methods : ast_method list; attributes : ast_attribute list}

let verify_expression(expr : ast_expression) : ast_expression = (
  expr
  (* TODO for full PA2 *)
)

let verify_parameter(param : ast_param) : ast_param = (
  param
)

let verify_method(mthd : ast_method) : ast_method = (
  let mthd = {mthd with params = List.map verify_parameter mthd.params} in
  let mthd = {mthd with body = verify_expression mthd.body} in
  mthd
)

let verify_attribute_no_init(attribute : ast_attribute) : ast_attribute = (
  attribute
)

let verify_attribute_init(attribute : ast_attribute) : ast_attribute = (
  attribute
)

let verify_attribute(attribute : ast_attribute) : ast_attribute = (
  match attribute with
  | AttributeNoInit { name : ast_identifier; _type  : ast_identifier } -> 
    (
      let attribute = verify_attribute_no_init attribute in
      attribute
    ) 
  | AttributeInit { name : ast_identifier; _type  : ast_identifier; init  : ast_expression; } -> (
      let attribute = verify_attribute_init attribute in
      attribute
    )
)

let verify_class(cls : ast_class) (class_env: class_environment) : ast_class = (
  (* Name and inherits are checked by verify_classes file *)
  let has_main lst = List.exists (fun (ast_method : ast_method) -> ast_method.name.name = "main") lst in
  if (cls.name.name = "Main") then (
    if (not (has_main class_env.methods)) then
    error_and_exit 0 "class Main method main not found";
  ); 

  let check_method_conflict (class_name : string) (method_to_check : ast_method) =
    match List.find_opt (fun (m : ast_method) -> m.name.name = method_to_check.name.name) class_env.methods with
    | Some existing_method ->
        if existing_method._type.name <> method_to_check._type.name  then (
          error_and_exit method_to_check.name.line_number ("class " ^ class_name ^ " redefines method " ^ method_to_check.name.name
        ^ " and changes return type (from " ^ existing_method._type.name ^ " to " ^ method_to_check._type.name ^ ")"); 
        )
        else if existing_method.params <> method_to_check.params then (
          if (List.length existing_method.params <> List.length method_to_check.params) then (
            error_and_exit method_to_check.name.line_number ("class " ^ class_name ^ " redefines method " ^ method_to_check.name.name 
            ^ " and changes number of formals")
          )
          else (
            error_and_exit method_to_check.name.line_number ("class " ^ class_name ^ " redefines method " ^ method_to_check.name.name 
            ^ " and changes type of formal")
          )
          )
    | None -> ()
  in

  (* Combining these two possible? *)
  let check_dupe_attribute (attrs: ast_attribute list) : unit =
    let seen = Hashtbl.create (List.length attrs) in
    let rec check_duplicate = function
      | [] -> ()
      | (AttributeNoInit { name; _ } | AttributeInit { name; _ }) :: rest ->
          if Hashtbl.mem seen name.name then (
          error_and_exit name.line_number ("class " ^ cls.name.name ^ " redefines attribute " ^ name.name);       
          )
          (* Is this correct location? *)
          else if name.name = "self" then (
            error_and_exit name.line_number ("class " ^ cls.name.name ^ " has an attribute named self");       
          )
          else (
            Hashtbl.add seen name.name ();
            check_duplicate rest
          )
    in
    check_duplicate attrs
  in

    let check_dupe_methods (mthds: ast_method list) : unit =
      let seen = Hashtbl.create (List.length mthds) in
      let rec check_duplicate = function
        | [] -> ()
        | ({ name; _; } : ast_method) :: rest ->
            if Hashtbl.mem seen name.name then (
            error_and_exit name.line_number ("class " ^ cls.name.name ^ " redefines method " ^ name.name);       
            )
            else (
              Hashtbl.add seen name.name ();
              check_duplicate rest
            )
      in
      check_duplicate mthds
  in

  check_dupe_methods cls.methods;
  check_dupe_attribute cls.attributes;
  let _ = List.map (check_method_conflict cls.name.name) cls.methods in
  let cls = { cls with attributes = List.map verify_attribute cls.attributes } in
  let cls = { cls with methods = List.map verify_method cls.methods} in
  cls
)

let rec create_type_environment(cls : ast_class) (full_ast : ast) : class_environment = (
  let find_class_by_name (name : string) (classes : ast_class list) : ast_class =
    List.find (fun (cls : ast_class) -> cls.name.name = name) classes 
  in
  let abort_method : ast_method = {
    name = { name = "abort"; line_number = 0 };
    params = [];
    _type = { name = "Object"; line_number = 0 };
    body = { ident = { name = "abort"; line_number = 0 }; data = Unreachable };
  }
  in
  let type_name_method : ast_method = {
    name = { name = "type_name"; line_number = 0 };
    params = [];
    _type = { name = "String"; line_number = 0 };
    body = { ident = { name = "type_name"; line_number = 0 }; data = Unreachable };
  }
  in
  let copy_method : ast_method = {
    name = { name = "copy"; line_number = 0 };
    params = [];
    _type = { name = "SELF_TYPE"; line_number = 0 };
    body = { ident = { name = "copy"; line_number = 0 }; data = Unreachable };
  }
  in  
  let object_methods : ast_method list = [ abort_method; type_name_method; copy_method;] in

  let out_string_method : ast_method = {
    name = { name = "out_string"; line_number = 0 };
    params = [];
    _type = { name = "SELF_TYPE"; line_number = 0 };
    body = { ident = { name = "abort"; line_number = 0 }; data = Unreachable };
  }
  in
  let out_int_method : ast_method = {
    name = { name = "out_int"; line_number = 0 };
    params = [];
    _type = { name = "SELF_TYPE"; line_number = 0 };
    body = { ident = { name = "type_name"; line_number = 0 }; data = Unreachable };
  }
  in
  let in_string_method : ast_method = {
    name = { name = "in_string"; line_number = 0 };
    params = [{
      name = { name = "x"; line_number = 0 };
      _type = { name = "String"; line_number = 0 };
    }];
    _type = { name = "String"; line_number = 0 };
    body = { ident = { name = "abort"; line_number = 0 }; data = Unreachable };
  }
  in
  let in_int_method : ast_method = {
    name = { name = "in_int"; line_number = 0 };
    params = [{
      name = { name = "x"; line_number = 0 };
      _type = { name = "Int"; line_number = 0 };
    }];
    _type = { name = "Int"; line_number = 0 };
    body = { ident = { name = "type_name"; line_number = 0 }; data = Unreachable };
  }
  in
  let io_methods : ast_method list = [ out_int_method; out_string_method; in_int_method; in_string_method;] in

  if (Option.is_some cls.inherits) then (
    if ((Option.get cls.inherits).name = "Object") then (
      { methods = cls.methods @ object_methods; attributes = cls.attributes }
    )
    else if ((Option.get cls.inherits).name = "IO") then (
      { methods = cls.methods @ io_methods; attributes = cls.attributes  }
    ) else (
      let parent_class = find_class_by_name (Option.get cls.inherits).name full_ast in
      let parent_type_env = create_type_environment parent_class full_ast in
      {methods = parent_type_env.methods @ cls.methods; attributes = parent_type_env.attributes @ cls.attributes}
    )
  ) else (
    { methods = cls.methods @ object_methods; attributes = cls.attributes}
  )
)

let verify_ast (ast : ast) : ast =  (
  let pass_environment cls : ast_class = (
    let env = create_type_environment cls ast in
    let cls = verify_class cls env in
    cls
  ) in
  let ast = List.map pass_environment ast  in
  ast
)
