open E_ast_data
open B_ast

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

(*
    Originally this was a more proper symbol table implementation that allowed scoping (i.e.
    pushing and popping scopes where popping a scope would automatically remove all variables
    added in that scope). The problem with this though is moreso a skill issue than anything else,
    because of the immutability of the symbol table, I would often misplace the reference and end
    up with misaligned scopes.

    Given COOL's compiler-friendliness just treating each case individually is much simpler, where
    I can just add and remove variables when parsing expressions without needing a automatic scoping
    method for doing so.
 *)
module Symbol_data = Hashtbl.Make(
    struct
        type t = string
        let equal = String.equal
        let hash = Hashtbl.hash
    end
)

type class_member =
    | Method        of ast_method
    | Attribute     of ast_attribute

type output_context = {
    classes : class_map;
    vars : string Symbol_data.t;

    current_method : string;
    current_class : string;
}

let get_member_line (member : class_member) : int =
    match member with
    | Method _method -> _method.name.line_number
    | Attribute _attr ->
        (match _attr with
        | AttributeNoInit { name; _type } -> name.line_number
        | AttributeInit { name; _type; init } -> name.line_number)

let add_variable (context : output_context) (name : string) (type_ : string) : unit =
    Symbol_data.add context.vars name type_

let remove_variable (context : output_context) (name : string) : unit =
    Symbol_data.remove context.vars name

let find_variable (context : output_context) (name : string) : string =
    match Symbol_data.find_opt context.vars name with
    | Some type_ -> type_
    | None -> raise (Failure ("Variable not found: " ^ name))

(* 
    This is very similar in functionality to the expression verification procedure in g_typecheck, this
    exists for two reasons:
        1. Typechecking and output were written in parallel, I didn't realize I could reuse that code,
        however this probably was the less headache-inducing option given the inevitable problems with
        trying to get a procedure of this size to work in two different contexts.

        2. For things such as SELF_TYPE, it feels as though having two separate procedures makes this
        process easier as well, given that there is a very specific set of rules for how the AST needs
        to be outputted, and there is some utility in treating SELF_TYPE as it's parent class when
        doing typechecking.
*)
let rec get_expr_type (context : output_context) (expr : ast_expression) : string =
    match expr.data with
    | Assign { rhs; _ } -> get_expr_type context rhs
    | DynamicDispatch { call_on; _method; _ } -> 
        let class_name = get_expr_type context call_on in
        let dispatch = 
            (get_dispatch context.classes 
             (upgrade_type class_name context.current_class) 
             _method.name) in
        upgrade_type (Option.get dispatch)._type.name @@ get_expr_type context call_on
    | StaticDispatch { call_on; _type; _method; _ } -> 
        let dispatch = 
            (get_static_dispatch context.classes _type.name _method.name) in
        upgrade_type (Option.get dispatch)._type.name @@ get_expr_type context call_on
    | SelfDispatch { _method; _ } -> 
        let dispatch = (get_dispatch context.classes context.current_class _method.name) in
        (Option.get dispatch)._type.name
    | If { _then; _else; _ } -> 
        let then_type = get_expr_type context _then in
        let else_type = get_expr_type context _else in

        join_classes context.current_class context.classes then_type else_type
    | While { predicate; _ } -> "Object"
    | Block { body } ->
        let rec last_type (l : ast_expression list) =
            match l with
            | [] -> raise (Failure "Empty block")
            | x :: [] -> get_expr_type context x
            | _ :: xs -> last_type xs
        in

        last_type body
    | New { _class } -> _class.name
    | IsVoid { expr } -> "Bool"
    | BinOp { op } -> 
        (match op with
        | Plus | Minus | Times | Divide -> "Int"
        | LT | LE | EQ -> "Bool"
        )
    | UnOp { op } ->
        (match op with
        | Not -> "Bool"
        | Negate -> "Int"
        )
    | Integer _ -> "Int"
    | String _ -> "String"
    | Identifier ident ->
        if ident.name = "self" then "SELF_TYPE"
        else find_variable context ident.name
    | True | False -> "Bool"
    | Let { bindings; _in } ->
        List.iter 
            (fun (binding : ast_let_binding_type) ->
                match binding with
                | LetBindingNoInit { variable; _type } -> add_variable context variable.name _type.name
                | LetBindingInit { variable; _type; value } -> add_variable context variable.name _type.name
            )
            bindings;
        
        let type_ = get_expr_type context _in in

        List.iter 
            (fun (binding : ast_let_binding_type) -> 
                match binding with
                | LetBindingNoInit { variable; _type } -> remove_variable context variable.name
                | LetBindingInit { variable; _type; value } -> remove_variable context variable.name
            )
            bindings;

        type_
    | Case { mapping_list } -> 
        let type_of (ast : ast_case_mapping) : string =
            add_variable context ast.name.name ast._type.name;
            let type_ = get_expr_type context ast.maps_to in
            remove_variable context ast.name.name;
            type_
        in

        let types = List.map type_of @@ List.tl mapping_list in
        List.fold_left 
            (fun x y -> join_classes context.current_class context.classes x y) 
            (type_of @@ List.hd mapping_list) 
            types
    | Unit -> "Object"
    | Unreachable -> "Object"
    | Internal _ ->
        let _method = get_dispatch context.classes context.current_class context.current_method in

        (Option.get _method)._type.name

let output_ast (ast : ast_data) (file_path : string) : unit =
    let oc = open_out file_path in

    (* Auxillary methods for printing to a file *)
    let output_line (line : string) = Printf.fprintf oc "%s\n" line in
    let output_number (num : int) = Printf.fprintf oc "%d\n" num in
    let output_identifier (ident : B_ast.ast_identifier) =
        output_number ident.line_number;
        output_line ident.name;
    in
    let output_typed_identifier (ident : B_ast.ast_identifier) (type_ : string) =
        output_number ident.line_number;
        output_line type_;
        output_line ident.name;
    in
    let output_list (l: 'a list) (for_each : 'a -> unit) =  
        output_number (List.length l);
        List.iter for_each l
    in

    let rec output_expression (expr : ast_expression) (context : output_context) : unit =
        output_typed_identifier   expr.ident (get_expr_type context expr);

        match expr.data with
        | Assign            { var; rhs } ->
            output_identifier   var;
            output_expression   rhs context;
        | DynamicDispatch   { call_on; _method; args } ->
            output_expression   call_on context;
            output_identifier   _method;
            output_list         args    (fun x -> output_expression x context)
        | StaticDispatch    { call_on; _type; _method; args } ->
            output_expression call_on context;
            output_identifier _type;
            output_identifier _method;
            output_list         args    (fun x -> output_expression x context)
        | SelfDispatch      { _method; args } ->
            output_identifier   _method;            
            output_list         args    (fun x -> output_expression x context)
        | If                { predicate; _then; _else } ->
            output_expression   predicate context;
            output_expression   _then    context;
            output_expression   _else    context
        | While             { predicate; body } -> 
            output_expression   predicate context;
            output_expression   body    context
        | Block             { body } ->
            output_list         body    (fun x -> output_expression x context)
        | New               { _class } ->
            output_identifier   _class
        | IsVoid            { expr } ->
            output_expression   expr context
        | BinOp             { left; right; _ } ->
            output_expression   left context;
            output_expression   right context;
        | UnOp              { expr; _ } ->
            output_expression   expr context
        | Integer i  ->
            output_number       i
        | String str ->
            output_line         str
        | Identifier ident ->
            output_identifier   ident
        | True -> ()
        | False -> ()
        | Let               { bindings; _in } ->
            let output_binding (binding : ast_let_binding_type) : unit =
                match binding with 
                | LetBindingNoInit      { variable; _type } ->
                    output_line         "let_binding_no_init";
                    output_identifier   variable;
                    output_identifier   _type;

                    add_variable       context variable.name _type.name;
                    ()
                | LetBindingInit        { variable; _type; value } ->
                    output_line         "let_binding_init";
                    output_identifier   variable;
                    output_identifier   _type;
                    output_expression   value context;

                    add_variable       context variable.name _type.name;
                    ()
            in

            output_number       (List.length bindings);
            List.iter           output_binding bindings;
            output_expression   _in context;

            List.iter 
                (fun (binding : ast_let_binding_type) -> 
                    match binding with
                    | LetBindingNoInit { variable; _type } -> remove_variable context variable.name
                    | LetBindingInit { variable; _type; value } -> remove_variable context variable.name
                )
                bindings
        | Case                  { expression; mapping_list } ->
            output_expression   expression context;

            let output_case_mapping (mapping : B_ast.ast_case_mapping) =
                output_identifier   mapping.name;
                output_identifier   mapping._type;

                add_variable context mapping.name.name mapping._type.name;
                output_expression   mapping.maps_to context;
                remove_variable context mapping.name.name;

                ()
            in

            output_number       (List.length mapping_list);
            List.iter           output_case_mapping mapping_list
        | Unreachable -> ()
        | Internal data -> output_line data
        | _ -> Printf.printf "Unhandled Expression!\n"; exit 1
    in

    let output_class_map (context : output_context) : unit =
        let output_attribute (_attr : ast_attribute) (context : output_context) : unit =
            match _attr with
            | AttributeNoInit   { name; _type } ->
                    output_line "no_initializer";
                    output_line name.name;
                    output_line _type.name;

                    add_variable context name.name _type.name
            | AttributeInit     { name; _type; init } ->
                    output_line "initializer";
                    output_line name.name;
                    output_line _type.name;
                    output_expression init context;

                    add_variable context name.name _type.name
        in

        let output_class _ (class_data : class_data) : unit =
            let context = { context with current_class = class_data.class_ref.name.name } in
            let attributes = get_attributes context.classes class_data.class_ref.name.name in
                        
            output_line class_data.class_ref.name.name;
            output_number (List.length attributes);
            List.iter (fun _attr -> output_attribute _attr context) attributes
        in

        output_line "class_map";
        output_number (StringMap.cardinal ast.classes);
        StringMap.iter output_class context.classes
    in

    let output_implementation_map (context : output_context) =
        let rec method_order (class_name : string) : (string * string) list =
            let _class = StringMap.find class_name context.classes in
            
            let super_methods = match class_name, _class.class_ref.inherits with
            | "Object", _ -> []
            | _, Some parent -> method_order parent.name
            | _, None        -> method_order "Object"
            in

            List.fold_left
                (fun acc (_method : ast_method) ->
                    if List.exists (fun (_, method_name) -> method_name = _method.name.name) acc then
                        List.map
                            (fun (super_class_name, method_name) ->
                                if method_name = _method.name.name then 
                                    (class_name, method_name) 
                                else 
                                    (super_class_name, method_name))
                            acc
                    else acc @ [(class_name, _method.name.name)]
                )
                super_methods
                _class.class_ref.methods
        in

        let output_method (context : output_context) (class_name : string) (method_name : string) : unit =
            let _method = StringMap.find method_name (StringMap.find class_name ast.classes).methods in
            let context = { context with 
                current_method = method_name; 
                current_class = class_name } in

            List.iter 
                (fun (param : ast_param) -> 
                    add_variable context param.name.name param._type.name; ()) 
                    _method.params;

            output_line method_name;
            
            output_number (List.length _method.params);
            List.iter (fun (param : ast_param) -> output_line param.name.name) _method.params;

            output_line class_name;        
            output_expression _method.body context;

            List.iter 
                (fun (param : ast_param) -> 
                    remove_variable context param.name.name; ())
                    _method.params
        in

        let output_class _ (class_data : class_data) : unit =
            let context = { context with current_class = class_data.class_ref.name.name; } in

            List.iter (fun (attr : ast_attribute) -> 
                match attr with
                | AttributeNoInit { name; _type } -> add_variable context name.name _type.name
                | AttributeInit { name; _type; _ } -> add_variable context name.name _type.name
                )
                @@ get_attributes context.classes class_data.class_ref.name.name
            ;

            let _class = class_data.class_ref in
            let ordered_methods = method_order _class.name.name in

            output_line _class.name.name;
            output_number @@ List.length ordered_methods;

            List.iter
                (fun (class_name, _method) -> output_method context class_name _method)
                ordered_methods;

            ()
        in

        output_line "implementation_map";
        output_number (StringMap.cardinal ast.classes);
        StringMap.iter output_class ast.classes
    in

    let output_parent_map (context : output_context) =
        let output_class _ (class_data : class_data) : unit =
            let name = class_data.class_ref.name.name in

            if name <> "Object" then (
                output_line name;
                match class_data.class_ref.inherits with
                | Some parent -> output_line parent.name
                | None -> output_line "Object"
            )
        in

        output_line "parent_map";
        (* Don't include Object *)
        output_number ((StringMap.cardinal ast.classes) - 1);
        StringMap.iter output_class ast.classes
    in

    let output_annotateB_ast (context : output_context) =
        let output_class (class_data : class_data) =
            let context = { context with 
                current_class = class_data.class_ref.name.name;
                vars = Symbol_data.create 10
            } in

            let output_body_expr (member : class_member) =
                match member with
                | Method _method ->
                    let context = { context with current_method = _method.name.name } in

                    output_line "method";
                    output_identifier _method.name;
                    output_number (List.length _method.params);

                    List.iter 
                        (fun (param : ast_param) -> 
                            output_identifier param.name;
                            output_identifier param._type;
                            add_variable context param.name.name param._type.name)
                        _method.params;
                    
                    output_identifier _method._type;
                    output_expression _method.body context;

                    List.iter 
                        (fun (param : ast_param) -> 
                            remove_variable context param.name.name)
                        _method.params
                | Attribute attr ->
                    (match attr with
                    | AttributeNoInit { name; _type } ->
                        output_line "attribute_no_init";
                        output_identifier name;
                        output_identifier _type;

                        add_variable context name.name _type.name
                    | AttributeInit { name; _type; init } ->
                        output_line "attribute_init";
                        output_identifier name;
                        output_identifier _type;
                        output_expression init context;
                        
                        add_variable context name.name _type.name)
            in

            List.iter (fun (attr : ast_attribute) -> 
                match attr with
                | AttributeNoInit { name; _type } -> add_variable context name.name _type.name
                | AttributeInit { name; _type; _ } -> add_variable context name.name _type.name
                )
                @@ get_attributes context.classes class_data.class_ref.name.name
            ;

            output_identifier class_data.class_ref.name;

            (match class_data.class_ref.inherits with
            | Some parent -> 
                output_line "inherits";
                output_identifier parent
            | None ->
                output_line "no_inherits"
            );

            let method_members = List.map 
                (fun (_method : ast_method) -> Method _method) 
                class_data.class_ref.methods
            in
            let attribute_members = List.map 
                (fun (_attr : ast_attribute) -> Attribute _attr) 
                class_data.class_ref.attributes
            in
            let body_members = method_members @ attribute_members in

            (* 
                I simply don't have the time to go back and reformat the ast to include both methods and
                attributes together, so this is a hacky way to get them back in order.
            *)
            let sorted = List.sort
                (fun body1 body2 ->
                    let line1 = get_member_line body1 in
                    let line2 = get_member_line body2 in

                    compare line1 line2
                )
                body_members 
            in

            output_number (List.length sorted);
            List.iter (output_body_expr) sorted;
        in

        let output_filter (class_data : class_data) =
            match class_data.class_ref.name.name with
            | "Object" | "IO" | "Int" | "String" | "Bool" -> ()
            | _ -> output_class class_data
        in

        (* Exclude 5 intrinsic class types *)
        output_number (StringMap.cardinal ast.classes - 5);

        let class_list = List.map snd @@ StringMap.bindings ast.classes in
        let sorted_list = List.sort
            (fun class1 class2 ->
                let line1 = class1.class_ref.name.line_number in
                let line2 = class2.class_ref.name.line_number in

                compare line1 line2
            )
            class_list
        in

        List.iter output_filter sorted_list
    in

    let context = {
        classes = ast.classes;
        vars = Symbol_data.create 10;
        current_method = "";
        current_class = "";
    } in

    output_class_map { context with vars = Symbol_data.create 10 };
    output_implementation_map { context with vars = Symbol_data.create 10 };
    output_parent_map { context with vars = Symbol_data.create 10 }; 
    output_annotateB_ast { context with vars = Symbol_data.create 10 }