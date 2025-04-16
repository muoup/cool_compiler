open A_util
open B_ast
open B_class_map
open B_impl_map
open C_parser_data
open D_tac_data
open E_expr_to_tac

let generate_constructor (data : program_data) (symbol_table : symbol_table ref) (_class : program_class_data) : method_tac =
    let temp_counter = ref 0 in
    let local_counter = ref 1 in

    let attributes = List.length _class.attributes in
    let object_id = Local 0 in

    let instantiate = TAC_object ( object_id, _class.name, attributes) in
    let return = TAC_return object_id in

    (* TODO: Attribute initialization *)

    let constructor_name = constructor_name_gen _class.name in

    let default_attr_cmds =
        _class.attributes
        |> List.mapi (
            fun (i : int) (attr : attribute_data) ->
                let attribute_id = index_of _class.attributes attr in

                match attr._type with
                | "String" -> 
                    [
                        TAC_comment ("StringNoInit: " ^ attr.name); 
                        TAC_default (Attribute attribute_id, "String"); 
                    ]
                | _ -> []
            )
        |> List.flatten
    in

    let valued_attr_cmds =
        _class.attributes
        |> List.mapi (
            fun (i : int) (attr : attribute_data) ->
                let attribute_id = index_of _class.attributes attr in

                match attr.init with
                | None -> []
                | Some init ->
                    let val_id, val_cmds = tac_gen_expr_body data _class.name attr._type init symbol_table local_counter temp_counter in

                    TAC_comment ("AttributeInit: " ^ attr.name) ::
                    val_cmds @ 
                    [ TAC_ident (Attribute attribute_id, val_id); ]
            )
        |> List.flatten
    in

    {
        class_name = _class.name;
        method_name = constructor_name;
        arg_count = 0;

        locals = !local_counter;
        temps = !temp_counter;

        commands = instantiate :: default_attr_cmds @ valued_attr_cmds @ [return];
    }

let generate_tac (data : program_data) : method_tac list =
    let symbol_table : symbol_table ref = ref @@ StringTbl.create 10 in

    let tac_class_impl (_class : program_class_data) : method_tac list =
        StringTbl.add !symbol_table "self" (Self, _class.name);

        List.iteri (
            fun i (attr : attribute_data) ->
                StringTbl.add !symbol_table attr.name (Attribute i, attr._type)
        ) _class.attributes;

        let constructor = generate_constructor data symbol_table _class in

        let tac_ast_method (method_ : impl_method) : method_tac =
            let _method = List.find (
                fun (data : impl_method) -> data.name = method_.name
            ) _class.methods in

            let temp_counter = ref 0 in
            let local_counter = ref 0 in

            (* Not sure how to replicate a Rust matches! macro, but this should work *)
            match method_.body.data with
            | Internal id ->
                {
                    class_name = _class.name;
                    method_name = method_name_gen _class.name method_.name;
                    arg_count = List.length method_.formals + 1;

                    locals = 0;
                    temps = 0;
                    
                    commands = [TAC_internal id];
                }
            | _ ->

            let return_type, params = get_method_signature data _class.name method_.name in

            List.iteri (
                fun i (name, _type) ->
                    StringTbl.add !symbol_table name (Parameter i, _type)
            ) @@ List.combine method_.formals params;

            let val_id, cmds = tac_gen_expr_body data 
                _class.name return_type 
                method_.body symbol_table 
                local_counter temp_counter in 

            List.iter (
                fun formal ->
                    StringTbl.remove !symbol_table formal
            ) method_.formals;

            {
                class_name = _class.name;
                method_name = method_name_gen _class.name method_.name;
                arg_count = List.length method_.formals + 1;

                locals = !local_counter;
                temps = !temp_counter;
                
                commands = cmds @ [TAC_return val_id];
            }  
        in

        let methods = List.map (tac_ast_method) _class.methods in

        List.iter (
            fun (attribute : attribute_data) ->
                StringTbl.remove !symbol_table attribute.name
        ) _class.attributes;

        StringTbl.remove !symbol_table "self";

        constructor :: methods
    in

    StringMap.fold (
        fun _ class_data acc ->
            let methods = tac_class_impl class_data in
            acc @ methods
    ) data.data_map []