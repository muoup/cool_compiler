open D_ast
open A_util
open E_symbol_map
module AstIdentifierSet = Set.Make(String)

(* TODO
Make sure symbol table is correctly updated everywhere, including attributes (IT IS DEFINITELY NOT RIGHT NOW)
Define "method environment M that is global to the entire program and defines for every class C
the signatures of all of the methods of C (including any inherited methods)."
Maintain some class hierarchy
Implement join / |_| and <= type operators
Typecheck dispatch (no I definitely didn't leave the hardest to last)
*)
let st = "SELF_TYPE"

let join (types : string list) : string = "Unimplemented join"

let lthan_eq (t1 : string) (t2 : string) : bool = true

let rec verify_expression(expr : ast_expression) (curr_class : ast_identifier) (symbol_map : symbol_map): string  = (
  match expr.data with

    | Assign { var : ast_identifier; rhs : ast_expression } -> (
        let rhs_type = verify_expression rhs curr_class symbol_map in
        let var_type = try get_symbol var.name symbol_map with Not_found -> 
        (error_and_exit var.line_number ("Identifier " ^ var.name ^ " was not initialized to a type")) in

        if (not (lthan_eq rhs_type var_type)) then 
          error_and_exit var.line_number ("Cannot assign variable " ^ var.name ^ " of type " ^ var_type 
          ^ " to an expression of type " ^ rhs_type);
        rhs_type
      )

    | DynamicDispatch { call_on : ast_expression; _method: ast_identifier; args : ast_expression list } -> (
      (* TODO *)
        "Unknown"
      )

    | StaticDispatch { call_on : ast_expression; _type : ast_identifier; _method : ast_identifier; args : ast_expression list } -> (
        "Unknown"
      )

    | SelfDispatch { _method : ast_identifier; args : ast_expression list } -> (
        curr_class.name
      )

    | If { predicate : ast_expression; _then : ast_expression; _else : ast_expression } -> (
        let predicate_type = verify_expression predicate curr_class symbol_map in
        if predicate_type <> "Bool" then
          error_and_exit expr.ident.line_number ("If statement has predicate of type " ^ predicate_type ^ " instead of Bool");
        let then_type = verify_expression _then curr_class symbol_map and
        else_type = verify_expression _else curr_class symbol_map in
        join [then_type; else_type]
      )

    | While { predicate : ast_expression; body : ast_expression } -> (
        let predicate_type = verify_expression predicate curr_class symbol_map in
        if predicate_type <> "Bool" then
          error_and_exit predicate.ident.line_number ("While loop has predicate of type" ^ predicate_type ^ "instead of Bool");
        let _ = verify_expression body curr_class symbol_map in
        "Object"
      )

    | Block { body : ast_expression list } -> (
        let all_types = List.map (fun e -> verify_expression e curr_class symbol_map) body in
        join all_types
      )

    | New { _class : ast_identifier } -> (
        if _class.name = st then curr_class.name else _class.name
      )

    | IsVoid { expr : ast_expression } -> (
        let _ = verify_expression expr curr_class symbol_map in
        "Bool"
      )

    | BinOp { left : ast_expression; right : ast_expression; op : ast_bin_op_type } -> (
        let left_type = verify_expression left curr_class symbol_map and
        right_type = verify_expression right curr_class symbol_map in
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
        let expr_type = verify_expression expr curr_class symbol_map in
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
      (* TODO *)
      (* 1. typecheck ast_let_binding_type
         2. put all the variables defined in let into the symbol table
         3. typecheck in with the new symbol table
         4. remove those variables from the symbol table *)
        "Unknown"
      )

    | True -> (
        "Bool"
      )

    | False -> (
        "Bool"
      )

    | Case { expression : ast_expression; mapping_list : ast_case_mapping list } -> (
      (* TODO *)
        "Unknown"
      )

    | Unit -> (
      (* TODO is this correct? *)
        "Unit"
      )

    | Unreachable -> (
        error_and_exit expr.ident.line_number "An unexpected type of expression was reached";
      )
      
    | Internal _ -> "Object"
)

let verify_parameter(param : ast_param) : ast_param = (
  param
)

let verify_method(mthd : ast_method) (curr_class : ast_identifier) (symbol_map : symbol_map): ast_method = (
  let mthd = {mthd with params = List.map verify_parameter mthd.params} in
  let method_type = verify_expression mthd.body curr_class symbol_map in
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
  let cls = { cls with attributes = List.map verify_attribute cls.attributes} in
  let cls = { cls with methods = List.map (fun mthd -> verify_method mthd cls.name (new_symbol_map ())) cls.methods} in
  cls
)

let verify_ast (ast : ast) : ast =  (
  let ast = List.map verify_class ast in
  ast
)