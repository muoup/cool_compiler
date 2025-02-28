open D_ast
open A_util
module AstIdentifierSet = Set.Make(String)
type typed_ast_expression = {
  _type: string;
  ident: ast_identifier;
  data: ast_expression_val
}

let rec verify_expression(expr : typed_ast_expression) : typed_ast_expression  = (
  match expr.data with
    | Assign { var : ast_identifier; rhs : ast_expression } -> (
        {_type = "Unknown"; ident = expr.ident; data = expr.data}
      )
    | DynamicDispatch { call_on : ast_expression; _method: ast_identifier; args : ast_expression list } -> (
        {_type = "Unknown"; ident = expr.ident; data = expr.data}
      )
    | StaticDispatch { call_on : ast_expression; _type : ast_identifier; _method : ast_identifier; args : ast_expression list } -> (
        {_type = "Unknown"; ident = expr.ident; data = expr.data}
      )
    | SelfDispatch { _method : ast_identifier; args : ast_expression list } -> (
        {_type = "Unknown"; ident = expr.ident; data = expr.data}
      )
    | If { predicate : ast_expression; _then : ast_expression; _else : ast_expression } -> (
        {_type = "Unknown"; ident = expr.ident; data = expr.data}
      )
    | While { predicate : ast_expression; body : ast_expression } -> (
        let typed_predicate = verify_expression {_type = "Unknown"; ident = predicate.ident; data = predicate.data} in
        if typed_predicate._type <> "Bool" then
          error_and_exit typed_predicate.ident.line_number ("While loop has predicate of type" ^ typed_predicate._type ^ "instead of Bool");
        let _ = verify_expression {_type = "Unknown"; ident = body.ident; data = body.data} in
        {_type = "Object"; ident = expr.ident; data = expr.data}
      )
    | Block { body : ast_expression list } -> (
        {_type = "Unknown"; ident = expr.ident; data = expr.data}
      )
    | New { _class : ast_identifier } -> (
        {_type = "Unknown"; ident = expr.ident; data = expr.data}
      )
    | IsVoid { expr : ast_expression } -> (
        {_type = "Unknown"; ident = expr.ident; data = expr.data}
      )
    | BinOp { left : ast_expression; right : ast_expression; op : ast_bin_op_type } -> (
        {_type = "Unknown"; ident = expr.ident; data = expr.data}
      )
    | UnOp { expr : ast_expression; op : ast_un_op_type } -> (
        {_type = "Unknown"; ident = expr.ident; data = expr.data}
      )
    | Integer int -> (
        {_type = "Unknown"; ident = expr.ident; data = expr.data}
      )
    | String string -> (
        {_type = "Unknown"; ident = expr.ident; data = expr.data}
      )
    | Identifier ast_identifier -> (
        {_type = "Unknown"; ident = expr.ident; data = expr.data}
      )
    | Let { bindings : ast_let_binding_type list; _in : ast_expression } -> (
        {_type = "Unknown"; ident = expr.ident; data = expr.data}
      )
    | True -> (
        {_type = "Bool"; ident = expr.ident; data = expr.data}
      )
    | False -> (
        {_type = "Bool"; ident = expr.ident; data = expr.data}
      )
    | Case { expression : ast_expression; mapping_list : ast_case_mapping list } -> (
        {_type = "Unknown"; ident = expr.ident; data = expr.data}
      )
    | Unit -> (
        {_type = "Unknown"; ident = expr.ident; data = expr.data}
      )
    | Unreachable -> (
        (error_and_exit : int -> string -> unit) expr.ident.line_number "An unexpected type of expression was reached";
        {_type = "Unknown"; ident = expr.ident; data = expr.data}
      )

)

let verify_parameter(param : ast_param) : ast_param = (
  param
)

let verify_method(mthd : ast_method) : ast_method = (
  (* I'm not exactly sure of the necessary output format so I'm doing this manipulation to make progress on getting code working.
     As interactions get more clear, this should be cleaned ups *)
  let mthd = {mthd with params = List.map verify_parameter mthd.params} in
  let untyped_body = {_type = "Unknown"; ident = mthd.body.ident; data = mthd.body.data} in
  let typechecked_body = verify_expression untyped_body in
  let new_body = {ident = typechecked_body.ident; data = typechecked_body.data} in
  let mthd = {mthd with body = new_body} in
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
  let cls = { cls with attributes = List.map verify_attribute cls.attributes } in
  let cls = { cls with methods = List.map verify_method cls.methods} in
  cls
)

let verify_ast (ast : ast) : ast =  (
  let ast = List.map verify_class ast in
  ast
)