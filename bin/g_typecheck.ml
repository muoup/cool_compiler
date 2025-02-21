open D_ast

let verify_expression(expr : ast_expression) : unit = (
  (* TODO for full PA2 *)
)

let verify_parameter(param : ast_param) : unit = ()

let verify_method(mthd : ast_method) : unit = (
  let _ = List.map verify_parameter mthd.params in
  let _ = verify_expression mthd.body in
  ()
)

let verify_attribute_no_init(attribute : ast_attribute) : unit = ()

let verify_attribute_init(attribute : ast_attribute) : unit = ()

let verify_attribute(attribute : ast_attribute) : unit = (
  match attribute with
  | AttributeNoInit { name : ast_identifier; _type  : ast_identifier } -> (verify_attribute_no_init attribute)
  | AttributeInit { name : ast_identifier; _type  : ast_identifier; init  : ast_expression; } -> (verify_attribute_init attribute)
)

let verify_class(cls : ast_class) : unit = (
  let _ = List.map verify_attribute cls.attributes in
  let _ = List.map verify_method cls.methods in
  ()
)

let verify_ast (ast : ast) : unit =  (
  let _ = List.map verify_class ast in ()
)
