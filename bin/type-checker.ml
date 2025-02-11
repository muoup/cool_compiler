(* For right now, I just want to test for correct types in operators *)

open Ast

exception Type_error

type ExpressionType =
    | Int
    | String
    | Bool
    | IO

let unimplemented =
    Printf.printf "Not yet implemented!\n"; raise Type_error

let rec verify_expression (expr : ast_expression) : ExpressionType =
    let verify_bin_op (left : ast_expression) (right : ast_expression) (op : ast_bin_op_type) =
        let left_type = verify_expression left in
        let right_type = verify_expression right in

        match left_type, right_type with
        | Int, Int -> Int
        | _, _ -> 
                Printf.printf "Invalid Binary Operation Pair"
                raise Type_error
    in

    match expr
    | BinOp { left right op }       -> verify_bin_op left right op 
    | String                        -> String
    | Integer                       -> Int
    | x                             -> unimplemented

let verify_body_expr (body_expr : ast_body_expr) =
    

    match body_expr with
    | Method
    | AttributeNoInit               -> unimplemented
    | AttributeInit                 -> unimplemented

let verify_class (_class : ast_class) = 
    List.map verify_body_expr _class.body_exprs

let verify_ast (ast : ast) = 
    List.map verify_class ast
