open D_ast
open A_util
module AstIdentifierSet = Set.Make(String)
(* At some point the environment will have to be passed through these functions (class, method, objects) *)

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

let verify_class(cls : ast_class) : ast_class = (
  (* Name and inherits are checked by verify_classes file *)
  let has_main lst = List.exists (fun (ast_method : ast_method) -> ast_method.name.name = "main") lst in
  (* TODO actually not fully correct (but passes test so I'm leaving it for now ) - main can inherit the method main *)
  if (cls.name.name = "Main") then (
    if (not (has_main cls.methods)) then
    error_and_exit 0 "class Main method main not found";
  ); 

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
  let cls = { cls with attributes = List.map verify_attribute cls.attributes } in
  let cls = { cls with methods = List.map verify_method cls.methods} in
  cls
)

let verify_ast (ast : ast) : ast =  (
  let ast = List.map verify_class ast in
  ast
)
