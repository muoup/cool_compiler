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
        let typed_rhs = verify_expression {_type = "Unknown"; ident = rhs.ident; data = rhs.data} in
        (* let var_type = lookup_in_symbol_table var  *)
        (* if rhs type > var type error *)
        {_type = typed_rhs._type; ident = expr.ident; data = expr.data}
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
        let typed_predicate = verify_expression {_type = "Unknown"; ident = predicate.ident; data = predicate.data} in
        if typed_predicate._type <> "Bool" then
          error_and_exit typed_predicate.ident.line_number ("If statement has predicate of type" ^ typed_predicate._type ^ "instead of Bool");
        let typed_then = verify_expression {_type = "Unknown"; ident = _then.ident; data = _then.data} in
        let typed_else = verify_expression {_type = "Unknown"; ident = _else.ident; data = _else.data} in
        (* return type is LUB of then and else *)
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
        (* Add to symbol table *)
        {_type = _class.name; ident = expr.ident; data = expr.data}
      )

    | IsVoid { expr : ast_expression } -> (
        let typed_expr = verify_expression {_type = "Unknown"; ident = expr.ident; data = expr.data} in
        {_type = "Bool"; ident = typed_expr.ident; data = typed_expr.data}
      )

    | BinOp { left : ast_expression; right : ast_expression; op : ast_bin_op_type } -> (
        let typed_left = verify_expression {_type = "Unknown"; ident = left.ident; data = left.data} in
        let typed_right = verify_expression {_type = "Unknown"; ident = right.ident; data = right.data} in
        let xor a b = (a || b) && not (a && b) in
        if (op = Plus || op = Minus || op = Times || op = Divide) then (
          if (typed_left._type <> "Int") then (
            error_and_exit typed_left.ident.line_number 
              ("Left side of an arithmetic operator must have type Int, not" ^ typed_left._type);
          )
          else if (typed_right._type <> "Int") then (
            error_and_exit typed_right.ident.line_number 
              ("Left side of an arithmetic operator must have type Int, not" ^ typed_right._type);
          )
          else 
            {_type = "Int"; ident = expr.ident; data = expr.data}
        )

        else if (op = LT || op = LE || op = EQ) then (
          if (xor (typed_left._type = "Int") (typed_right._type = "Int")) then
            error_and_exit typed_left.ident.line_number "An Int may only be compared with another Int";
          if (xor (typed_left._type = "String") (typed_right._type = "String")) then
            error_and_exit typed_left.ident.line_number "A String may only be compared with another String";
          if (xor (typed_left._type = "Bool") (typed_right._type = "Bool")) then
            error_and_exit typed_left.ident.line_number "An Int may only be compared with another Int";
          {_type = "Bool"; ident = expr.ident; data = expr.data}
        ) else (
          {_type = "Unknown"; ident = expr.ident; data = expr.data}
        )
      )

    | UnOp { expr : ast_expression; op : ast_un_op_type } -> (
        let typed_expr = verify_expression {_type = "Unknown"; ident = expr.ident; data = expr.data} in
        if op = Negate then (
          if typed_expr._type <> "Int" then 
            error_and_exit typed_expr.ident.line_number ("Negate operator can only be used on Int, not " ^ typed_expr._type);
        );
        if op = Not then (
          if typed_expr._type <> "Bool" then 
            error_and_exit typed_expr.ident.line_number ("Not operator can only be used on Bool, not " ^ typed_expr._type);
        );
        {_type = typed_expr._type; ident = typed_expr.ident; data = typed_expr.data}
      )

    | Integer _ -> (
        {_type = "Int"; ident = expr.ident; data = expr.data}
      )

    | String _ -> (
        {_type = "String"; ident = expr.ident; data = expr.data}
      )

    | Identifier ast_identifier -> (
        (* Look up type in symbol table and return that *)
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
        {_type = "Unit"; ident = expr.ident; data = expr.data}
      )

    | Unreachable -> (
        error_and_exit expr.ident.line_number ("An unexpected type of expression " ^ expr._type ^  " was reached";)
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