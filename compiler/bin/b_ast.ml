open A_parser

type ast_bin_op_type =
    | Plus
    | Minus
    | Times
    | Divide
    | LT
    | LE
    | EQ

and ast_un_op_type =
    | Not
    | Negate

and ast_case_mapping = {
    name            : ast_identifier;
    _type           : ast_identifier;
    maps_to         : ast_expression;
}

and ast_expression = {
    ident: ast_identifier;
    _type: string;
    data: ast_expression_val
}

and ast_let_binding_type =
    | LetBindingNoInit      of { variable : ast_identifier; _type : ast_identifier;                         }
    | LetBindingInit        of { variable : ast_identifier; _type : ast_identifier; value : ast_expression; }

and ast_expression_val =
    | Assign                of { var        : ast_identifier; rhs       : ast_expression }
    | DynamicDispatch       of { call_on    : ast_expression; _method   : ast_identifier; args      : ast_expression list }
    | StaticDispatch        of { call_on    : ast_expression; _type     : ast_identifier; _method   : ast_identifier; args   : ast_expression list }
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
    | Let                   of { bindings : ast_let_binding_type list; _in : ast_expression }
    | True             
    | False      
    | Case                  of { expression : ast_expression; mapping_list : ast_case_mapping list }
    | Internal              of string

and ast_param =                { name : ast_identifier; _type : ast_identifier }

and ast_dispatch_type =
    | SelfDispatch
    | DynamicDispatch

and ast_method =               { name : ast_identifier; params : ast_param list; _type : ast_identifier; body : ast_expression; }

and ast_attribute =
    | AttributeNoInit       of { name : ast_identifier; _type  : ast_identifier }
    | AttributeInit         of { name : ast_identifier; _type  : ast_identifier; init  : ast_expression; }

and ast_body_expr =
    | Method                of ast_method
    | AttributeNoInit       of ast_attribute
    | AttributeInit         of ast_attribute

and ast_class = {
    name        : ast_identifier;
    inherits    : ast_identifier option;
    attributes  : ast_attribute list;
    methods     : ast_method list;
}

and ast  = ast_class list

let rec parse_expression (data : parser_data) : (parser_data * ast_expression) =
    let parse_assignment (data : parser_data) : (parser_data * ast_expression_val) =
        let data, ident = parse_identifier data in
        let data, rval  = parse_expression data in

        data, Assign { var = ident; rhs = rval }
    in

    let parse_dyn_dispatch (data : parser_data) : (parser_data * ast_expression_val) =
        let data, call_on     = parse_expression data in
        let data, _method     = parse_identifier data in
        let data, args        = parse_list data parse_expression in

        data, DynamicDispatch { call_on = call_on; _method = _method; args = args }
    in

    let parse_static_dispatch (data : parser_data) : (parser_data * ast_expression_val) =
        let data, call_on     = parse_expression data in
        let data, _type       = parse_identifier data in
        let data, _method     = parse_identifier data in
        let data, args        = parse_list data parse_expression in

        data, StaticDispatch  { call_on = call_on; _type = _type; _method = _method; args = args }
    in

    let parse_self_dispatch (data : parser_data) : (parser_data * ast_expression_val) =
        let data, method_name = parse_identifier data in
        let data, call_params = parse_list data parse_expression in
        
        data, SelfDispatch { _method = method_name; args =  call_params }
    in

    let parse_if_statement (data : parser_data) : (parser_data * ast_expression_val) =
        let data, predicate = parse_expression data in
        let data, _then     = parse_expression data in
        let data, _else     = parse_expression data in

        data, If { predicate = predicate; _then = _then; _else = _else }
    in

    let parse_while_loop (data : parser_data) : (parser_data * ast_expression_val) =
        let data, predicate = parse_expression data in
        let data, body      = parse_expression data in

        data, While { predicate = predicate; body = body }
    in

    let parse_block (data : parser_data) : (parser_data * ast_expression_val) =
        let data, body = parse_list data parse_expression in

        data, Block { body = body }
    in

    let parse_new (data : parser_data) : (parser_data * ast_expression_val) =
        let data, ident = parse_identifier data in

        data, New { _class = ident }
    in

    let parse_is_void (data : parser_data) : (parser_data * ast_expression_val) =
        let data, expr = parse_expression data in

        data, IsVoid { expr = expr }
    in

    let parse_bin_op (data : parser_data) (op_type : ast_bin_op_type) : (parser_data * ast_expression_val) =
        let data, left = parse_expression data in
        let data, right = parse_expression data in

        data, BinOp { left = left; right = right; op = op_type }
    in

    let parse_un_op (data : parser_data) (op_type : ast_un_op_type) : (parser_data * ast_expression_val) =
        let data, expr = parse_expression data in

        data, UnOp { expr = expr; op = op_type }
    in

    let parse_int_expr (data : parser_data) : (parser_data * ast_expression_val) =
        let data, val_ = parse_int data in
    
        data, Integer val_ 
    in

    let parse_string (data : parser_data) : (parser_data * ast_expression_val) =
        let data, val_ = parse_line data in

        data, String val_
    in

    let parse_identifier_expr (data : parser_data) : (parser_data * ast_expression_val) =
        let data, identifier = parse_identifier data in

        data, Identifier identifier
    in

    let parse_let (data : parser_data) : (parser_data * ast_expression_val) =
        let parse_let_binding (data : parser_data) : (parser_data * ast_let_binding_type) =
            let data, type_ident   = parse_line data in 
            let data, variable     = parse_identifier data in
            let data, _type        = parse_identifier data in
    
            match type_ident with
            | "let_binding_init" ->
                    let data, value       = parse_expression data in
                    data, LetBindingInit    { variable; _type; value }
            | "let_binding_no_init" ->
                    data, LetBindingNoInit  { variable; _type }
            | x -> raise (Invalid_argument (Printf.sprintf "Invalid let binding type: %s" x))
        in

        let data, binding_list = parse_list data parse_let_binding in
        let data, _in          = parse_expression data in
        data, Let { bindings = binding_list; _in }
    in

    let parse_case (data : parser_data) : (parser_data * ast_expression_val) =
        let parse_case_mapping (data : parser_data) : (parser_data * ast_case_mapping) =
            let data, var_name = parse_identifier data in
            let data, var_type = parse_identifier data in
            let data, maps_to  = parse_expression data in

            data, { name = var_name; _type = var_type; maps_to = maps_to }
        in

        let data, expr     = parse_expression data in
        let data, mappings = parse_list data parse_case_mapping in
        
        data, Case { expression = expr; mapping_list = mappings }
    in

    let data, line_number = parse_int data in
    let data, value_type = parse_line data in
    let data, expr_type = parse_line data in

    let data, expression = match expr_type with
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
    | "case"                -> parse_case data
    | "internal"            -> 
            let data, identifier = parse_line data in
            data, Internal identifier
    | unsupported -> 
            Printf.printf "Unsupported expression: %s\n" unsupported;
            raise Ast_error
    in

    data, { ident = { line_number = line_number; name = expr_type }; _type = value_type; data = expression }

let parse_ast (data : parser_data) : (parser_data * ast) =
    let parse_parameters (data : parser_data) : (parser_data * ast_param list) =
        let parse_param (data : parser_data) : (parser_data * ast_param) =
            let data, param_name    = parse_identifier data in
            let data, _type         = parse_identifier data in

            data, { name = param_name; _type = _type }
        in

        parse_list data parse_param
    in

    let parse_method (data : parser_data) : (parser_data * ast_method) =
        let data, method_name   = parse_identifier data in
        let data, params        = parse_parameters data in
        let data, return_type   = parse_identifier data in
        let data, body          = parse_expression data in
 
        (data, {
            name = method_name;
            params = params;
            _type = return_type;
            body = body;
        })
    in

    let parse_attribute_no_init (data : parser_data) : (parser_data * ast_attribute) =
        let data, attribute_name    = parse_identifier data in
        let data, _type             = parse_identifier data in

        (data, AttributeNoInit { name = attribute_name; _type = _type })
    in

    let parse_attribute_init (data : parser_data) : (parser_data * ast_attribute) =
        let data, attribute_name    = parse_identifier data in
        let data, _type             = parse_identifier data in
        let data, init_expr         = parse_expression data in

        (data, AttributeInit { name = attribute_name; _type = _type; init = init_expr }) 
    in

    let parse_body_expr (data : parser_data) (_class : ast_class) : (parser_data * ast_class) =
        let data, body_expr_type = parse_line data in

        match body_expr_type with
        | "method" ->
            let data, _method = parse_method data in
            data, { _class with methods = _class.methods @ [_method] }
        | "attribute_no_init" ->
            let data, attribute = parse_attribute_no_init data in
            data, { _class with attributes = _class.attributes @ [attribute] }
        | "attribute_init" ->
            let data, attribute = parse_attribute_init data in
            data, { _class with attributes = _class.attributes @ [attribute] }
        | invalid -> 
                Printf.printf "Invalid Body Expression Type: %s at line %d\n" invalid data.line_number;
                raise Ast_error
    in

    let parse_class (data : parser_data) : (parser_data * ast_class) =
        let data, class_name = parse_identifier data in 
        let data, inherits_text = parse_line data in
        let data, inherits = 
            match inherits_text with
            | "inherits" ->
                    let data, identifier = parse_identifier data in
                    data, Some identifier
            | "no_inherits" ->
                    data, None
            | x -> 
                Printf.printf "Unexpected inherits: %s" x;
                raise Ast_error
        in
        let data, body_exprs = parse_int data in
        let _class = {
            name = class_name;
            inherits = inherits;
            attributes = [];
            methods = [];
        } in

        let rec consume_n_body_exprs data _class n =
            match n with
            | 0 -> data, _class
            | i -> 
                let data, _class = parse_body_expr data _class in
                consume_n_body_exprs data _class (i - 1)
        in

        consume_n_body_exprs data _class body_exprs
    in

    let _, classes = parse_list data parse_class in
    
    let ast = List.sort 
        (fun (class1 : ast_class) (class2 : ast_class) -> compare class1.name class2.name) 
        classes
    in

    (data, ast)