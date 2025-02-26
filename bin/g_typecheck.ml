open D_ast
open A_util
module AstIdentifierSet = Set.Make(String)
type object_environment = {classes : ast_class list}
type class_environment = {methods : ast_method list; attributes : ast_attribute list}
type variable = {name : string; _type: string}
type method_environment = {variables : variable list;}


let verify_expression(expr : ast_expression) : ast_expression = (
  match expr.data with
    | Assign { var : ast_identifier; rhs : ast_expression } -> (expr)
    | DynamicDispatch { call_on : ast_expression; _method: ast_identifier; args : ast_expression list } -> (expr)
    | StaticDispatch { call_on : ast_expression; _type : ast_identifier; _method : ast_identifier; args : ast_expression list } -> (expr)
    | SelfDispatch { _method : ast_identifier; args : ast_expression list } -> (expr)
    | If { predicate : ast_expression; _then : ast_expression; _else : ast_expression } -> (expr)
    | While { predicate : ast_expression; body : ast_expression } -> (expr)
    | Block { body : ast_expression list } -> (expr)
    | New { _class : ast_identifier } -> (expr)
    | IsVoid { expr : ast_expression } -> (expr)
    | BinOp { left : ast_expression; right : ast_expression; op : ast_bin_op_type } -> (expr)
    | UnOp { expr : ast_expression; op : ast_un_op_type } -> (expr)
    | Integer int -> (expr)
    | String string -> (expr)
    | Identifier ast_identifier -> (expr)
    | Let { bindings : ast_let_binding_type list; _in : ast_expression } -> (expr)
    | True -> (expr)
    | False -> (expr)
    | Case { expression : ast_expression; mapping_list : ast_case_mapping list } -> (expr)
    | Unit -> (expr)
    | Unreachable -> (expr)

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

let verify_class(cls : ast_class): ast_class = (
  let cls = { cls with attributes = List.map verify_attribute cls.attributes } in
  let cls = { cls with methods = List.map verify_method cls.methods} in
  cls
)

let verify_ast (ast : ast) : ast =  (
  let ast = List.map verify_class ast in
  ast
)