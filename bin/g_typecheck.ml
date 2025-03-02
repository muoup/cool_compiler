open D_ast
open A_util
open E_symbol_map
module AstIdentifierSet = Set.Make(String)
module StringMap = Map.Make(String)

type method_data = { 
    methods : ast_method StringMap.t;
}
type class_methods_map = method_data StringMap.t

let st = "SELF_TYPE"

let join (types : string list) : string = "Unimplemented join"

let lthan_eq (t1 : string) (t2 : string) : bool = true

let rec verify_expression(expr : ast_expression) (curr_class : ast_identifier) (symbol_map : symbol_map)
  (method_env : class_methods_map) : string  = (

  match expr.data with

    | Assign { var : ast_identifier; rhs : ast_expression } -> (
        let rhs_type = verify_expression rhs curr_class symbol_map method_env in
        let var_type = try get_symbol var.name symbol_map with Not_found -> 
        (error_and_exit var.line_number ("Identifier " ^ var.name ^ " was not initialized to a type")) in

        if (not (lthan_eq rhs_type var_type)) then 
          error_and_exit var.line_number ("Cannot assign variable " ^ var.name ^ " of type " ^ var_type 
          ^ " to an expression of type " ^ rhs_type);
        rhs_type
      )

    | DynamicDispatch { call_on : ast_expression; _method: ast_identifier; args : ast_expression list } -> (
        "Unknown"
      )

    | StaticDispatch { call_on : ast_expression; _type : ast_identifier; _method : ast_identifier; args : ast_expression list } -> (
        "Unknown"
      )

    | SelfDispatch { _method : ast_identifier; args : ast_expression list } -> (
        curr_class.name
      )

    | If { predicate : ast_expression; _then : ast_expression; _else : ast_expression } -> (
        let predicate_type = verify_expression predicate curr_class symbol_map method_env in
        if predicate_type <> "Bool" then
          error_and_exit expr.ident.line_number ("If statement has predicate of type " ^ predicate_type ^ " instead of Bool");
        let then_type = verify_expression _then curr_class symbol_map method_env and
        else_type = verify_expression _else curr_class symbol_map method_env in
        join [then_type; else_type]
      )

    | While { predicate : ast_expression; body : ast_expression } -> (
        let predicate_type = verify_expression predicate curr_class symbol_map method_env in
        if predicate_type <> "Bool" then
          error_and_exit predicate.ident.line_number ("While loop has predicate of type" ^ predicate_type ^ "instead of Bool");
        let _ = verify_expression body curr_class symbol_map method_env in
        "Object"
      )

    | Block { body : ast_expression list } -> (
        let all_types = List.map (fun e -> verify_expression e curr_class symbol_map method_env) body in
        join all_types
      )

    | New { _class : ast_identifier } -> (
        if _class.name = st then curr_class.name else _class.name
      )

    | IsVoid { expr : ast_expression } -> (
        let _ = verify_expression expr curr_class symbol_map method_env in
        "Bool"
      )

    | BinOp { left : ast_expression; right : ast_expression; op : ast_bin_op_type } -> (
        let left_type = verify_expression left curr_class symbol_map method_env and
        right_type = verify_expression right curr_class symbol_map method_env in
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
          error_and_exit expr.ident.line_number "Unexpected BinOp";
        )
      )

    | UnOp { expr : ast_expression; op : ast_un_op_type } -> (
        let expr_type = verify_expression expr curr_class symbol_map method_env in
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
      let id_type = try get_symbol ast_identifier.name symbol_map
        with Not_found -> 
          (error_and_exit ast_identifier.line_number ("Identifier " ^ ast_identifier.name ^ " was not initialized to a type")) in
      id_type
      )

    | Let { bindings : ast_let_binding_type list; _in : ast_expression } -> (
      let rec typecheck_bindings (bindings : ast_let_binding_type list) (map : symbol_map) : symbol_map = (
      match bindings with 
      | [] -> (
          map
        )
      | bd :: rest -> (
        match bd with
        LetBindingNoInit { variable : ast_identifier; _type : ast_identifier; } -> (
          if (_type.name = st) then
            add_symbol variable.name curr_class.name map
          else
            add_symbol variable.name _type.name map
          )
    |   LetBindingInit { variable : ast_identifier; _type : ast_identifier; value : ast_expression; } -> (
          let real_type = if (_type.name = st) then curr_class.name else _type.name in
          let value_type = verify_expression value curr_class map method_env in
          if real_type <> value_type then
            error_and_exit variable.line_number ("Variable " ^ variable.name ^ " of type " ^ _type.name ^ 
            " cannot be assigned to expression of type " ^ value_type);
            add_symbol variable.name real_type map
          )
        )
      ) in 
      let updated_symbol_table = typecheck_bindings bindings symbol_map in
      verify_expression _in curr_class updated_symbol_table method_env
      )

    | True -> (
        "Bool"
      )

    | False -> (
        "Bool"
      )

    | Case { expression : ast_expression; mapping_list : ast_case_mapping list } -> (
        let _ = verify_expression expression curr_class symbol_map method_env in
        let verify_mapping (mapping : ast_case_mapping) : string = (
          let updated_symbol_table = add_symbol mapping.name.name mapping._type.name symbol_map in
          verify_expression mapping.maps_to curr_class updated_symbol_table method_env
        ) in
        join (List.map verify_mapping mapping_list)
      )

    | Unit -> (
      (* TODO is this correct? *)
        "Unit"
      )

    | Unreachable -> (
        error_and_exit expr.ident.line_number "An unexpected type of expression was reached";
      )

)

let verify_parameter(param : ast_param) : ast_param = (
  param
)

let verify_method(mthd : ast_method) (curr_class : ast_identifier) (symbol_map : symbol_map)
  (method_env : class_methods_map): ast_method = (
  let mthd = {mthd with params = List.map verify_parameter mthd.params} in

  let rec add_params_to_symbol_table (params : ast_param list) (map : symbol_map) : symbol_map = 
    match params with 
    | [] -> ( symbol_map )
    | param :: rest -> (
      let map = add_symbol param.name.name param._type.name map in
      add_params_to_symbol_table rest map
    )
  in
  let symbol_map = add_params_to_symbol_table mthd.params symbol_map in
  let method_type = verify_expression mthd.body curr_class symbol_map method_env in
  {mthd with _type = {name = method_type; line_number = mthd.name.line_number}}
)

let verify_attribute(attribute : ast_attribute) (curr_class : ast_identifier) (symbols : symbol_map) 
 (method_env : class_methods_map)= (
  match attribute with
  | AttributeNoInit { name : ast_identifier; _type  : ast_identifier } -> 
    (
      (* Do nothing, has already been added to the symbol table   *)
    ) 
  | AttributeInit { name : ast_identifier; _type  : ast_identifier; init  : ast_expression; } -> (
      let _ = verify_expression init curr_class symbols method_env in
      ()
    )
)

let rec construct_class_symbol_map (attributes : ast_attribute list) (curr : symbol_map) : symbol_map = 
  (* No duplicate attributes are ensured by previous checks *)
  match attributes with
  | [] -> (
    curr
  )
  | attribute :: rest -> (
    match attribute with
    | AttributeNoInit { name : ast_identifier; _type  : ast_identifier } -> 
      (
        let curr = add_symbol name.name _type.name curr in
        construct_class_symbol_map rest curr
      ) 
    | AttributeInit { name : ast_identifier; _type  : ast_identifier; _ } -> 
      (
        let curr = add_symbol name.name _type.name curr in
        construct_class_symbol_map rest curr
      )  
  )

let verify_class(cls : ast_class) (method_env : class_methods_map) : ast_class = (
  let empty_symbol_map = new_symbol_map () in
  let class_symbol_map = construct_class_symbol_map cls.attributes empty_symbol_map in
  List.iter (fun a -> verify_attribute a cls.name (class_symbol_map) method_env) cls.attributes;
  let cls = { cls with methods = List.map (fun mthd -> verify_method mthd cls.name (class_symbol_map) method_env) cls.methods} in
  cls
)

let extract_method_map (cm : E_ast_data.class_map) : class_methods_map =
  StringMap.map (fun (c : E_ast_data.class_data) -> { methods = c.methods }) cm

let verify_ast (ast : ast) (class_data : E_ast_data.class_map) : ast =  (
  (* It feels like there should be a way to define this globally and not have to
  pass it through everywhere - I'll spend time figuring that out when everything is done *)
  let method_environment = extract_method_map class_data in
  let ast = List.map (fun c -> verify_class c method_environment) ast in
  ast
)