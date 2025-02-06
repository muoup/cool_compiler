(* AST Definition *)
type ast_identifier = string

type ast_expression =
    | Assign                of { var : ast_identifier; rhs : ast_expression }
    | DynamicDispatch       of { e   : ast_expression; _method : ast_identifier; args: ast_expression list }
    | StaticDispatch        of { e   : ast_expression; _type   : ast_identifier; _method: ast_identifier; args: ast_expression list }
    | SelfDispatch          of { _method : ast_identifier; args : ast_expression list }
    | If                    of { predicate : ast_expression; _then : ast_expression; _else : ast_expression }
    | While                 of { predicate : ast_expression; body : ast_expression }
    | Block                 of { body : ast_expression list }
    | New                   of { _class : ast_identifier }
    | IsVoid                of { e : ast_expression }
    | Plus                  of { x : ast_expression; y : ast_expression }
    | Minus                 of { x : ast_expression; y : ast_expression }
    | Times                 of { x : ast_expression; y : ast_expression }
    | Divide                of { x : ast_expression; y : ast_expression }
    | LT                    of { x : ast_expression; y : ast_expression }
    | LE                    of { x : ast_expression; y : ast_expression }
    | EQ                    of { x : ast_expression; y : ast_expression }
    | Not                   of { x : ast_expression; }
    | Negate                of { x : ast_expression; }
    | Integer               of int
    | String                of string
    | Identifier            of ast_identifier
    | True             
    | False      
    | LetBindingNoInit      of { variable : ast_identifier; _type : ast_identifier }
    | LetBindingInit        of { variable : ast_identifier; _type : ast_identifier; value : ast_expression }
    | Case                  of { expression : ast_expression; case_list : ast_expression list; body_list : ast_expression list }

type ast_formal =              { name : ast_identifier; _type : ast_identifier }

type ast_feature =
    | AttributeNoInit       of { name : ast_identifier; _type       : ast_identifier ; }
    | AttributeInit         of { name : ast_identifier; _type       : ast_identifier ; init : ast_expression; }
    | Method                of { name : ast_identifier; formals     : ast_formal list; body : ast_expression; }

type ast_class_type =
    | NoInherits
    | Inherits              of string

type ast_class = {
    class_type  : ast_class_type;
    name        : string;

    features    : ast_feature list;
}

type ast = ast_class list

(* Ast Parsing *)
let parse_ast (file_contents : string list) -> ast =
    match file_contents with
    | [] -> []











