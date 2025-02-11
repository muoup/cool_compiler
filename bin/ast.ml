type ast_identifier = { name : string; line_number: int; }

type ast_bin_op_type =
    | Plus
    | Minus
    | Times
    | Divide
    | LT
    | LE
    | EQ

type ast_un_op_type =
    | Not
    | Negate

type ast_expression =
    | Assign                of { var        : ast_identifier; rhs       : ast_expression }
    | DynamicDispatch       of { call_on    : ast_expression; _method   : ast_identifier; args   : ast_expression list }
    | StaticDispatch        of { call_on    : ast_expression; _method   : ast_identifier; args   : ast_expression list }
    | SelfDispatch          of { _method    : ast_identifier; args      : ast_expression list }
    | If                    of { predicate  : ast_expression; _then     : ast_expression; _else : ast_expression }
    | While                 of { predicate  : ast_expression; body      : ast_expression }
    | Block                 of { body       : ast_expression list }
    | New                   of { _class     : ast_identifier }
    | IsVoid                of { expr       : ast_expression }
    | BinOp                 of { left       : ast_expression; right     : ast_expression; op : ast_bin_op_type }
    | UnOp                  of { expr       : ast_expression; op        : ast_un_op_type }
    | Integer               of int
    | String                of string
    | Identifier            of ast_identifier
    | True             
    | False      
    | LetBindingNoInit      of { variable : ast_identifier; _type : ast_identifier;                         _in : ast_expression }
    | LetBindingInit        of { variable : ast_identifier; _type : ast_identifier; value : ast_expression; _in : ast_expression }
    | Case                  of { expression : ast_expression; case_list : ast_expression list; body_list : ast_expression list }

type ast_param =               { name : ast_identifier; _type : ast_identifier }

type ast_dispatch_type =
    | SelfDispatch
    | DynamicDispatch

type ast_body_expr =
    | Method                of { name : ast_identifier; params : ast_param list; _type : ast_identifier; body : ast_expression; } 
    | AttributeNoInit       of { name : ast_identifier; _type  : ast_identifier }
    | AttributeInit         of { name : ast_identifier; _type  : ast_identifier; init  : ast_expression; } 

type ast_class = {
    name        : ast_identifier;
    inherits    : ast_identifier option;
    body_exprs  : ast_body_expr list;
}

type ast  = ast_class list

exception Ast_error

type parser_data = {
    file_contents       : string list;
}

(* Ast Parsing *)
let pop_data_lines (data : parser_data) (n : int) : parser_data =
    { file_contents = Util.ntail data.file_contents n }

(* int_of_string wrapper that logs an error when given invalid input *)
let parse_int (str : string) : int =
    match int_of_string_opt str with
    | Some x -> x
    | None ->
        Printf.printf "Unknown line number: %s\n" str;
        raise Ast_error

let parse_list (data : parser_data) (mapping : parser_data -> (parser_data * 'a)) : (parser_data * 'a list) =
    let rec internal_rec data mapping (i : int) (acc : 'a list) : (parser_data * 'a list) =
        match i with
        | 0 -> (data, acc)
        | i -> 
            let data, produced = mapping data in
            let data, acc = internal_rec data mapping (i - 1) acc in

            (data, produced :: acc)
    in

    let line_count = parse_int (List.hd data.file_contents) in
    let data = pop_data_lines data 1 in

    internal_rec data mapping line_count [] 

let parse_ast (file_contents : string list) : ast =
    let parse_identifier (data : parser_data) : (parser_data * ast_identifier) =
        let identifier = List.nth data.file_contents 1 in
        let line_number = parse_int (List.hd data.file_contents) in

        (pop_data_lines data 2, { name = identifier; line_number = line_number })
    in

    let rec parse_expression (data : parser_data) : (parser_data * ast_expression) =
        
        let parse_assignment (data : parser_data) : (parser_data * ast_expression) =
            let data, ident = parse_identifier data in
            let data, rval  = parse_expression data in

            data, Assign { var = ident; rhs = rval }
        in

        let parse_dyn_dispatch (data : parser_data) : (parser_data * ast_expression) =
            let data, call_on     = parse_expression data in
            let data, _method     = parse_identifier data in
            let data, args        = parse_list data parse_expression in

            data, DynamicDispatch { call_on = call_on; _method = _method; args = args }
        in

        let parse_static_dispatch (data : parser_data) : (parser_data * ast_expression) =
            let data, call_on     = parse_expression data in
            let data, _method     = parse_identifier data in
            let data, args        = parse_list data parse_expression in

            data, StaticDispatch  { call_on = call_on; _method = _method; args = args }
        in

        let parse_self_dispatch (data : parser_data) : (parser_data * ast_expression) =
            let data, method_name = parse_identifier data in
            let data, call_params = parse_list data parse_expression in
            
            data, SelfDispatch { _method = method_name; args =  call_params }
        in

        let parse_if_statement (data : parser_data) : (parser_data * ast_expression) =
            let data, predicate = parse_expression data in
            let data, _then     = parse_expression data in
            let data, _else     = parse_expression data in

            data, If { predicate = predicate; _then = _then; _else = _else }
        in

        let parse_while_loop (data : parser_data) : (parser_data * ast_expression) =
            let data, predicate = parse_expression data in
            let data, body      = parse_expression data in

            data, While { predicate = predicate; body = body }
        in

        let parse_block (data : parser_data) : (parser_data * ast_expression) =
            let data, body = parse_list data parse_expression in

            data, Block { body = body }
        in

        let parse_new (data : parser_data) : (parser_data * ast_expression) =
            let data, ident = parse_identifier data in

            data, New { _class = ident }
        in

        let parse_is_void (data : parser_data) : (parser_data * ast_expression) =
            let data, expr = parse_expression data in

            data, IsVoid { expr = expr }
        in

        let parse_bin_op (data : parser_data) (op_type : ast_bin_op_type) : (parser_data * ast_expression) =
            let data, left = parse_expression data in
            let data, right = parse_expression data in

            data, BinOp { left = left; right = right; op = op_type }
        in

        let parse_un_op (data : parser_data) (op_type : ast_un_op_type) : (parser_data * ast_expression) =
            let data, expr = parse_expression data in

            data, UnOp { expr = expr; op = op_type }
        in

        let parse_int_expr (data : parser_data) : (parser_data * ast_expression) =
            let num = parse_int (List.hd data.file_contents) in
            let data = pop_data_lines data 1 in

            data, Integer num
        in

        let parse_string (data : parser_data) : (parser_data * ast_expression) =
            let str_val = List.hd data.file_contents in
 
            (pop_data_lines data 1, String str_val)
        in

        let parse_identifier_expr (data : parser_data) : (parser_data * ast_expression) =
            let data, identifier = parse_identifier data in

            data, Identifier identifier
        in
        
        let parse_let (data: parser_data) : (parser_data * ast_expression) =
            let parse_no_init (data : parser_data) : (parser_data * ast_expression) =
                let data, var     = parse_identifier data in
                let data, _type   = parse_identifier data in
                let data, _in     = parse_expression data in
                
                data, LetBindingNoInit { variable = var; _type = _type; _in = _in }
            in

            let parse_init (data : parser_data) : (parser_data * ast_expression) =
                let data, var     = parse_identifier data in
                let data, _type   = parse_identifier data in
                let data, value   = parse_expression data in
                let data, _in     = parse_expression data in

                data, LetBindingInit { variable = var; _type = _type; value = value; _in = _in }
            in

            let data, init = parse_identifier data in

            match init.name with
            | "let_binding_no_init"         -> parse_no_init data
            | "let_binding_init"            -> parse_init data
            | x                             -> Printf.printf "invalid let type: %s" init.name; raise Ast_error 
        in

        let data, expr_type = parse_identifier data in

        match expr_type.name with
        | "assign"              -> parse_assignment data
        | "dynamic_dispatch"    -> parse_dyn_dispatch data
        | "static_dispatch"     -> parse_static_dispatch data
        | "self_dispatch"       -> parse_self_dispatch data
        | "if"                  -> parse_if_statement data
        | "while"               -> parse_while_loop data
        | "block"               -> parse_block data
        | "new"                 -> parse_new data
        | "isvoid"              -> parse_is_void data
        | "plus"                -> parse_bin_op data Plus
        | "minus"               -> parse_bin_op data Minus
        | "times"               -> parse_bin_op data Times
        | "divide"              -> parse_bin_op data Divide
        | "lt"                  -> parse_bin_op data LT
        | "le"                  -> parse_bin_op data LE
        | "eq"                  -> parse_bin_op data EQ
        | "not"                 -> parse_un_op data Not
        | "negate"              -> parse_un_op data Negate
        | "integer"             -> parse_int_expr data
        | "string"              -> parse_string data
        | "identifier"          -> parse_identifier_expr data
        | "true"                -> data, True
        | "false"               -> data, False
        | "let"                 -> parse_let data
        | unsupported -> 
                Printf.printf "Unsupported expression: %s\n" unsupported;
                raise Ast_error
    in

    let parse_parameters (data : parser_data) : (parser_data * ast_param list) =
        let parse_param (data : parser_data) : (parser_data * ast_param) =
            let data, param_name    = parse_identifier data in
            let data, _type         = parse_identifier data in

            data, { name = param_name; _type = _type }
        in

        parse_list data parse_param
    in

    let parse_method (data : parser_data) : (parser_data * ast_body_expr) =
        (* Feels like there should be some way to do this with a monad *)
        let data, method_name   = parse_identifier data in
        let data, params        = parse_parameters data in
        let data, return_type   = parse_identifier data in
        let data, body          = parse_expression data in
 
        (data, Method {
            name = method_name;
            params = params;
            _type = return_type;
            body = body;
        })
    in

    let parse_attribute_no_init (data : parser_data) : (parser_data * ast_body_expr) =
        let data, attribute_name    = parse_identifier data in
        let data, _type             = parse_identifier data in

        (data, AttributeNoInit { name = attribute_name; _type = _type })
    in

    let parse_attribute_init (data : parser_data) : (parser_data * ast_body_expr) =
        let data, attribute_name    = parse_identifier data in
        let data, _type             = parse_identifier data in
        let data, init_expr         = parse_expression data in

        (data, AttributeInit { name = attribute_name; _type = _type; init = init_expr }) 
    in

    let parse_body_expr (data : parser_data) : (parser_data * ast_body_expr) =
        let body_expr_type = List.hd data.file_contents in
        let data = pop_data_lines data 1 in

        match body_expr_type with
        | "method" -> parse_method data
        | "attribute_no_init" -> parse_attribute_no_init data
        | _ -> raise Ast_error
    in

    let parse_class (data : parser_data) : (parser_data * ast_class) =
        let data, class_name = parse_identifier data in
        let data, inherits = 
            match (List.hd data.file_contents) with
            | "inherits" ->
                    let data = pop_data_lines data 1 in
                    let data, identifier = parse_identifier data in
                    data, Some identifier
            | "no_inherits" ->
                    pop_data_lines data 1, None
            | x -> 
                Printf.printf "Unexpected inherits: %s" x;
                raise Ast_error
        in
        let body_expr_count = parse_int (List.hd data.file_contents) in
        let data, body_exprs = parse_list data parse_body_expr in

        data, {
            name = class_name;
            inherits = inherits;
            body_exprs = body_exprs;
        }
    in

    let data = {
        file_contents = file_contents;
    } in

    let _, classes = parse_list data parse_class in
    classes
