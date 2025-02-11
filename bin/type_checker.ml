(* For right now, I just want to test for correct types in operators *)

open Ast

exception Type_error

type expression_type =
    | Int
    | String
    | Bool
    | IO

let unimplemented message =
    Printf.printf "Not yet implemented! %s\n" message; raise Type_error

let error message =
    Printf.printf "%s\n" message;
    raise Type_error

let rec verify_expression (expr : ast_expression) : expression_type =
    let verify_bin_op (left : ast_expression) (right : ast_expression) (op : ast_bin_op_type) =
        let left_type = verify_expression left in
        let right_type = verify_expression right in

        match left_type, right_type with
        | Int, Int -> Int
        | _, _ -> error "Invalid Binary Operator Pair"
    in

    match expr with
    | BinOp { left; right; op }     -> verify_bin_op left right op 
    | String _                      -> String
    | Integer _                     -> Int
    | x                             -> unimplemented "Misc Expression"

let verify_body_expr (body_expr : ast_body_expr) =
    match body_expr with
    | Method { body; _ }            -> verify_expression body
    | AttributeNoInit _             -> unimplemented "AttributeNoInit"
    | AttributeInit _               -> unimplemented "AttributeInit"

let verify_class (_class : ast_class) = 
    List.map verify_body_expr _class.body_exprs

let verify_ast (ast : ast) = 
    List.map verify_class ast;
    ()
