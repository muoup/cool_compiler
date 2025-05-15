open A_util
open B_ast
open B_class_map
open B_impl_map
open C_parser_data
open D_tac_data
open E_expr_to_tac

let generate_constructor (data : program_data) (symbol_table : symbol_table ref) (_class : program_class_data) : method_tac =
    List.iteri (
        fun i (attr : attribute_data) ->
            StringTbl.add !symbol_table attr.name (Attribute i, attr._type)
    ) _class.attributes;
    
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

    List.iteri (
        fun i (attr : attribute_data) ->
            StringTbl.remove !symbol_table attr.name;
    ) _class.attributes;

    {
        class_name = _class.name;
        method_name = constructor_name;
        arg_count = 0;

        locals = !local_counter;
        temps = !temp_counter;

        commands = instantiate :: default_attr_cmds @ valued_attr_cmds @ [return];
    }

let coalesce_intrinsics (data : program_data) : impl_method list =
    let get_method (class_name : string) (method_name : string) : impl_method =
        let _class = StringMap.find class_name data.data_map in
        let _method = List.find (fun (_method : impl_method) -> _method.name = method_name) _class.methods in

        _method
    in

    [
        get_method "Object" "abort";
        get_method "Object" "type_name";
        get_method "Object" "copy";

        get_method "String" "length";
        get_method "String" "concat";
        get_method "String" "substr";

        get_method "IO" "out_int";
        get_method "IO" "in_int";
        get_method "IO" "out_string";
        get_method "IO" "in_string";
    ]

let coalesce_implemented (data : program_data) : impl_method list =
    data.ast
    |> List.map (
        fun (_class : ast_class) ->
            let _impl_class = StringMap.find _class.name.name data.data_map in

            _class.methods
            |> List.map (
                fun (_method : ast_method) ->
                    _impl_class.methods
                    |> List.find (
                            fun (_impl_method : impl_method) -> 
                                _impl_method.name = _method.name.name
                        )
                )
        )
    |> List.concat

let generate_tac (data : program_data) : method_tac list =
    let symbol_table : symbol_table ref = ref @@ StringTbl.create 10 in

    (* Note that int/bool/string are not inheritable, so self will never get from them *)
    StringTbl.add !symbol_table "self" (Self, "");

    let implemented = coalesce_implemented data in
    let intrins = coalesce_intrinsics data in

    let generate_tac (_method : impl_method) : method_tac =
        let _class = StringMap.find _method.parent data.data_map in

        List.iteri (
            fun i (attr : attribute_data) ->
                StringTbl.add !symbol_table attr.name (Attribute i, attr._type)
        ) _class.attributes;

        let temp_counter = ref 0 in
        let local_counter = ref 0 in

        (* Not sure how to replicate a Rust matches! macro, but this should work *)
        match _method.body.data with
        | Internal id ->
            {
                class_name = _class.name;
                method_name = method_name_gen _class.name _method.name;
                arg_count = List.length _method.formals + 1;

                locals = 0;
                temps = 0;
                
                commands = [TAC_internal id];
            }
        | _ ->

        let return_type, params = get_method_signature data _class.name _method.name in

        List.iteri (
            fun i (name, _type) ->
                StringTbl.add !symbol_table name (Parameter i, _type)
        ) @@ List.combine _method.formals params;

        let val_id, cmds = tac_gen_expr_body data 
            _class.name return_type 
            _method.body symbol_table 
            local_counter temp_counter in 

        List.iter (
            fun formal ->
                StringTbl.remove !symbol_table formal
        ) _method.formals;

        {
            class_name = _class.name;
            method_name = method_name_gen _class.name _method.name;
            arg_count = List.length _method.formals + 1;

            locals = !local_counter;
            temps = !temp_counter;
            
            commands = cmds @ [TAC_return val_id];
        } 
    in

    let constructors = 
        data.data_map
        |> StringMap.bindings
        |> List.map (
            fun (_, _class) ->
                generate_constructor data symbol_table _class
            )
    in

    List.map (generate_tac) implemented @ constructors

let generate_tac_old (data : program_data) : method_tac list =
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