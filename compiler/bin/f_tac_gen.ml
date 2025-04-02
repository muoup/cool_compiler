open A_util
open B_ast
open B_class_map
open B_impl_map
open C_parser_data
open D_tac_data
open E_tac_expr_gen

let generate_constructor (_class : program_class_data) : method_tac =
    let attributes = List.length _class.attributes in

    let instantiate = TAC_object ("object", _class.name, attributes) in
    let return = TAC_return "object" in
    
    (* TODO: Attribute initialization *)

    let constructor_name = constructor_name_gen _class.name in

    {
        class_name = _class.name;
        method_name = constructor_name;
        arg_count = 0;

        commands = [ instantiate; return ];
        ids = [ "object" ]
    }

let generate_tac (data : program_data) : method_tac list =
    let symbol_table = ref @@ StringTbl.create 10 in

    StringTbl.add !symbol_table "self" "self";

    let add_symbol (x : string) : unit =
        StringTbl.add !symbol_table x x
    in

    let remove_symbol (x : string) : unit =
        StringTbl.remove !symbol_table x
    in

    let tac_class_impl (_class : program_class_data) : method_tac list =
        List.iter (fun (attr : attribute_data) -> add_symbol attr.name) _class.attributes;

        let tac_ast_method (method_ : impl_method) : (tac_id list * tac_cmd list) =
            let _method = List.find (
                fun (data : impl_method) -> data.name = method_.name
            ) _class.methods in

            (* Not sure how to replicate a Rust matches! macro, but this should work *)
            match method_.body.data with
            | Internal id -> [], [TAC_internal id]
            | _ ->

            List.iter (add_symbol) method_.formals;

            let cmds = tac_gen_expr_body data _class.name method_.body symbol_table in

            List.iter (remove_symbol) method_.formals;

            cmds
        in

        let methods = List.map (
            fun (method_ : impl_method) -> 
                let ids, cmds = tac_ast_method method_ in
                {
                    class_name = _class.name;
                    method_name = method_name_gen _class.name method_.name;
                    arg_count = List.length method_.formals;
                    
                    commands = cmds;
                    ids = ids;
                }   
        ) _class.methods in

        List.iter (fun (attr : attribute_data) -> remove_symbol attr.name) _class.attributes;

        generate_constructor _class :: methods
    in

    StringMap.fold (
        fun _ class_data acc ->
            let methods = tac_class_impl class_data in
            acc @ methods
    ) data.data_map []