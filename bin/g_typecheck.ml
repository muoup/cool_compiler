open D_ast

let verify_expression(expr : ast_expression) : ast_expression = (
  expr
  (* TODO for full PA2 *)
)

let verify_parameter(param : ast_param) : ast_param = (
  param
)

let verify_method(mthd : ast_method) : ast_method = (
  let _ = List.map verify_parameter mthd.params in
  let _ = verify_expression mthd.body in
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
  | AttributeNoInit { name : ast_identifier; _type  : ast_identifier } -> (verify_attribute_no_init attribute)
  | AttributeInit { name : ast_identifier; _type  : ast_identifier; init  : ast_expression; } -> (verify_attribute_init attribute)
)

let verify_class(cls : ast_class) : ast_class = (
  let cls = { cls with attributes = List.map verify_attribute cls.attributes } in
  let cls = { cls with methods = List.map verify_method cls.methods} in
  cls
)

let verify_ast (ast : ast) : ast =  (
  let ast = List.map verify_class ast in
  ast
)
