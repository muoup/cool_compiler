open Ast

exception Unimplemented_error

let unimplemented message = Printf.printf "Unimplemented %s\n" message; raise Unimplemented_error

let verify_class (cls : ast_class) = (unimplemented "class")

let verify_ast (ast : ast) =  List.map verify_class ast;