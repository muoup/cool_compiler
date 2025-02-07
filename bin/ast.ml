type ast_identifier = { name : string; line_number: int; }

type ast_expression =
    | Assign                of { var        : ast_identifier; rhs    : ast_expression }
    | DynamicDispatch       of { _method    : ast_identifier; args   : ast_expression list }
    | StaticDispatch        of { _method    : ast_identifier; params : ast_expression list }
    | SelfDispatch          of { _method    : ast_identifier; params : ast_expression list }
    | If                    of { predicate  : ast_expression; _then  : ast_expression; _else : ast_expression }
    | While                 of { predicate  : ast_expression; body   : ast_expression }
    | Block                 of { body       : ast_expression list }
    | New                   of { _class     : ast_identifier }
    | IsVoid                of { e          : ast_expression }
    | Plus                  of { left       : ast_expression; right  : ast_expression }
    | Minus                 of { left       : ast_expression; right  : ast_expression }
    | Times                 of { left       : ast_expression; right  : ast_expression }
    | Divide                of { left       : ast_expression; right  : ast_expression }
    | LT                    of { left       : ast_expression; right  : ast_expression }
    | LE                    of { left       : ast_expression; right  : ast_expression }
    | EQ                    of { left       : ast_expression; right  : ast_expression }
    | Not                   of { left       : ast_expression; }
    | Negate                of { left       : ast_expression; }
    | Integer               of int
    | String                of string
    | Identifier            of ast_identifier
    | True             
    | False      
    | LetBindingNoInit      of { variable : ast_identifier; _type : ast_identifier }
    | LetBindingInit        of { variable : ast_identifier; _type : ast_identifier; value : ast_expression }
    | Case                  of { expression : ast_expression; case_list : ast_expression list; body_list : ast_expression list }

type ast_param = { _type : ast_identifier; name : ast_identifier }

type ast_dispatch_type =
    | SelfDispatch
    | DynamicDispatch

type ast_body_expr =
    | Method                of { name : ast_identifier; params : ast_param list; _type : ast_identifier; } 
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
    classes_remaining   : int;
    accumulator         : ast;
}

(* Ast Parsing *)
let pop_data_lines (data : parser_data) (n : int) : parser_data =
    { data with file_contents = Util.ntail data.file_contents n }

(* int_of_string wrapper that logs an error when given invalid input *)
let parse_int (str : string) : int =
    match int_of_string_opt str with
    | Some x -> x
    | None ->
        Printf.printf "Unknown line number: %s\n" str;
        raise Ast_error

let parse_ast (file_contents : string list) : ast =
    let parse_identifier (data : parser_data) : (parser_data * ast_identifier) =
        let identifier = List.nth data.file_contents 1 in
        let line_number = parse_int (List.hd data.file_contents) in
        
        Printf.printf "Identifier parsed: %s\n" identifier;

        (pop_data_lines data 2, { name = identifier; line_number = line_number })
    in

    let rec parse_expression (data : parser_data) : (parser_data * ast_expression) =
        let parse_string (data : parser_data) : (parser_data * ast_expression) =
            let str_val = List.hd data.file_contents in
            Printf.printf "parse_string: %s\n" str_val;
 
            (pop_data_lines data 1, String str_val)
        in

        let parse_call_params (data : parser_data) : (parser_data * ast_expression list) =
            Printf.printf "parse_call_params: %s\n" (List.hd data.file_contents);
            
            let rec internal_rec (data : parser_data) (acc : ast_expression list) (i : int) =
                match i with
                | 0 -> (data, acc)
                | x -> 
                    let data, expr = parse_expression data in
                    internal_rec data (acc @ [expr]) (i - 1)
            in

            let param_count = parse_int (List.hd data.file_contents) in
            internal_rec (pop_data_lines data 1) [] param_count
        in

        let parse_self_dispatch (data : parser_data) : (parser_data * ast_expression) =
            Printf.printf "parse_self_dispatch: %s\n" (List.hd data.file_contents);

            let data, method_name = parse_identifier data in
            let data, call_params = parse_call_params data in

            data, SelfDispatch {
                _method = method_name;
                params = call_params;
            }
        in

        Printf.printf "parse_expression: %s\n" (List.hd data.file_contents);

        let data, expr_type = parse_identifier data in

        match expr_type.name with
        | "self_dispatch"   -> parse_self_dispatch data
        | "string"          -> parse_string data
        | unsupported -> 
            Printf.printf "Unsupported expression: %s\n" unsupported;
            raise Ast_error
    in

    (* TODO: Implement parameter parsing, right now this will only work with no parameter methods (such as in hello_world.cl) *)
    let parse_parameters (data : parser_data) : (parser_data * ast_param list) =
        Printf.printf "parse_parameters: %s\n" (List.hd data.file_contents);

        let rec internal_rec (data : parser_data) (acc : ast_param list) (i : int) : (parser_data * ast_param list) =
            match i with
            | 0 -> (data, acc)
            | x ->
                let data, param_name = parse_identifier data in
                let data, param_type = parse_identifier data in
                let param = { name = param_name; _type = param_type } in

                internal_rec data (acc @ [param]) (i - 1)
        in

        let count = int_of_string (List.hd data.file_contents) in
        let data = pop_data_lines data 1 in
        let data, params = internal_rec data [] count in

        (data, params)
    in

    let parse_method (data : parser_data) : (parser_data * ast_body_expr) =
        Printf.printf "parse_method: %s\n" (List.hd data.file_contents);

        (* Feels like there should be some way to do this with a monad *)
        let data, method_name   = parse_identifier data in
        let data, params        = parse_parameters data in
        let data, return_type   = parse_identifier data in
        let data, expression    = parse_expression data in
 
        (data, Method {
            name = method_name;
            params = params;
            _type = return_type;
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
        Printf.printf "parse_body_expr: %s\n" (List.hd data.file_contents);
        let body_expr_type = List.hd data.file_contents in
        let data = pop_data_lines data 1 in

        match body_expr_type with
        | "method" -> parse_method data
        | "attribute_no_init" -> parse_attribute_no_init data
        | _ -> raise Ast_error
    in

    let rec parse_body_exprs (data : parser_data) (acc : ast_body_expr list) (count : int) : (parser_data * ast_body_expr list) =
        Printf.printf "parse_body_exprs\n";

        match count with
        | 0 -> (data, acc)
        | x ->
            let data, body_expr = parse_body_expr data in
            parse_body_exprs data (acc @ [body_expr]) (count - 1)
    in

    let parse_class (data : parser_data) : parser_data =
        Printf.printf "parse_class: %s\n" (List.hd data.file_contents);

        let data, class_name = parse_identifier data in

        let data, inherits = 
            match (List.hd data.file_contents) with
            | "inherits" ->
                    let data = pop_data_lines data 1 in
                    let data, identifier = parse_identifier data in
                    data, Some identifier
            | "no_inherits" ->
                    pop_data_lines data 1, None
        in

        let body_expr_count = parse_int (List.hd data.file_contents) in
        let data, body_exprs = parse_body_exprs (pop_data_lines data 1) [] body_expr_count in

        let ast_class = {
            name = class_name;
            inherits = inherits;
            body_exprs = body_exprs;
        } in

        { 
            file_contents = file_contents; 
            classes_remaining = data.classes_remaining - 1; 
            accumulator = data.accumulator @ [ast_class]; 
        }
    in

    let rec internal_rec (data : parser_data) : parser_data =
        Printf.printf "internal_rec\n";

        match data.classes_remaining with
        | 0 -> data
        | _ -> 
            let data = parse_class data in
            internal_rec data
    in

    let class_count = parse_int (List.hd file_contents) in
    let file_contents = List.tl file_contents in

    let data = {
        file_contents = file_contents;
        classes_remaining = class_count;
        accumulator = []
    } in

    (internal_rec data).accumulator
