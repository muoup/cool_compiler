open D_ast
open A_util
module AstIdentifierSet = Set.Make(String)

let rec verify_expression(expr : ast_expression) : string  = (
  match expr.data with

    | Assign { var : ast_identifier; rhs : ast_expression } -> (
        let rhs_type = verify_expression rhs in
        (* let var_type = lookup_in_symbol_table var  *)
        (* if rhs type > var type error *)
          rhs_type
      )

    | DynamicDispatch { call_on : ast_expression; _method: ast_identifier; args : ast_expression list } -> (
        "Unknown"
      )

    | StaticDispatch { call_on : ast_expression; _type : ast_identifier; _method : ast_identifier; args : ast_expression list } -> (
      "Unknown"
      )

    | SelfDispatch { _method : ast_identifier; args : ast_expression list } -> (
      "Unknown"
      )

    | If { predicate : ast_expression; _then : ast_expression; _else : ast_expression } -> (
        let predicate_type = verify_expression predicate in
        if predicate_type <> "Bool" then
          error_and_exit expr.ident.line_number ("If statement has predicate of type " ^ predicate_type ^ " instead of Bool");
        let then_type = verify_expression _then and
        else_type = verify_expression _else in
        (* return type is LUB of then and else *)
        "Unknown"
      )

    | While { predicate : ast_expression; body : ast_expression } -> (
        let predicate_type = verify_expression predicate in
        if predicate_type <> "Bool" then
          error_and_exit predicate.ident.line_number ("While loop has predicate of type" ^ predicate_type ^ "instead of Bool");
        let _ = verify_expression body in
        "Object"
      )

    | Block { body : ast_expression list } -> (
        (* I think return type is LUB of all expression *)
        "Unknown"
      )

    | New { _class : ast_identifier } -> (
        (* Add to symbol table *)
        (* If self type, return correct class *)
        _class.name
      )

    | IsVoid { expr : ast_expression } -> (
        let _ = verify_expression expr in
        "Bool"
      )

    | BinOp { left : ast_expression; right : ast_expression; op : ast_bin_op_type } -> (
        let left_type = verify_expression left and
        right_type = verify_expression right in
        let xor a b = (a || b) && not (a && b) in
        if (op = Plus || op = Minus || op = Times || op = Divide) then (
          if (left_type <> "Int") then (
            error_and_exit expr.ident.line_number 
              ("Left side of an arithmetic operator must have type Int, not " ^ left_type);
          )
          else if (right_type <> "Int") then (
            error_and_exit expr.ident.line_number 
              ("Left side of an arithmetic operator must have type Int, not " ^ right_type);
          )
          else 
            "Int"
        )

        else if (op = LT || op = LE || op = EQ) then (
          if (xor (left_type = "Int") (right_type = "Int")) then
            error_and_exit expr.ident.line_number "An Int may only be compared with another Int";
          if (xor (left_type = "String") (right_type = "String")) then
            error_and_exit expr.ident.line_number "A String may only be compared with another String";
          if (xor (left_type = "Bool") (right_type = "Bool")) then
            error_and_exit expr.ident.line_number "An Int may only be compared with another Int";
          "Bool"
        ) else (
          (* TODO throw error *)
          "Unknown"
        )
      )

    | UnOp { expr : ast_expression; op : ast_un_op_type } -> (
        let expr_type = verify_expression expr in
        if op = Negate then (
          if expr_type <> "Int" then 
            error_and_exit expr.ident.line_number ("Negate operator can only be used on Int, not " ^ expr_type);
        );
        if op = Not then (
          if expr_type <> "Bool" then 
            error_and_exit expr.ident.line_number ("Not operator can only be used on Bool, not " ^ expr_type);
        );
        expr_type
      )

    | Integer _ -> (
        "Int"
      )

    | String _ -> (
        "String"
      )

    | Identifier ast_identifier -> (
        (* Look up type in symbol table and return that *)
        "Unknown"
      )

    | Let { bindings : ast_let_binding_type list; _in : ast_expression } -> (
        "Unknown"
      )

    | True -> (
        "Bool"
      )

    | False -> (
        "Bool"
      )

    | Case { expression : ast_expression; mapping_list : ast_case_mapping list } -> (
        "Unknown"
      )

    | Unit -> (
      (* TODO is this correct? *)
        "Unit"
      )

    | Unreachable -> (
        error_and_exit expr.ident.line_number "An unexpected type of expressio was reached";
      )

)

let verify_parameter(param : ast_param) : ast_param = (
  param
)

let verify_method(mthd : ast_method) : ast_method = (
  let mthd = {mthd with params = List.map verify_parameter mthd.params} in
  let method_type = verify_expression mthd.body in
  {mthd with _type = {name = method_type; line_number = mthd.name.line_number}}
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