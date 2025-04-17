open A_util
open B_ast
open B_class_map
open B_impl_map
open C_parser_data
open D_ssa_data
open E_expr_to_ssa

let generate_constructor (data : program_data) (_class : program_class_data) : method_ssa =
    let temp_counter = ref 0 in
    let local_counter = ref 0 in

    let create_stmt stmt =
        let id = Local !temp_counter in
        temp_counter := !temp_counter + 1;
        { id; _val = stmt }
    in

    let attributes = List.length _class.attributes in

    let instantiate = create_stmt @@ SSA_object (_class.name, attributes) in
    let return = create_stmt @@ SSA_return instantiate.id in
    
    (* TODO: Attribute initialization *)

    let constructor_name = constructor_name_gen _class.name in

    let attributes = List.map (
        fun (attr : attribute_data) ->
            let attribute_id = index_of _class.attributes attr in
            let object_id = Local 0 in

            match attr.init with
            | None ->
                let default = create_stmt @@ SSA_default (attr._type) in
                
                [
                    default; create_stmt @@ SSA_attribute { object_id; attribute_id; value = default.id }
                ]
            | Some init ->
                let (ssa_ids, ssa_cmds) = ssa_gen_expr_body data _class.name init (ref (StringTbl.create 10)) local_counter temp_counter in
                let ret = last_id ssa_ids in

                ssa_cmds @ [
                    create_stmt @@ SSA_attribute { object_id; attribute_id; value = ret }
                ]
    ) _class.attributes in

    {
        class_name = _class.name;
        method_name = constructor_name;
        arg_count = 0;

        stmts = instantiate :: (List.concat attributes) @ [return];
        ids = []
    }

let generate_ssa (data : program_data) : method_ssa list =
    let symbol_table : symbol_table ref = ref @@ StringTbl.create 10 in

    StringTbl.add !symbol_table "self" Self;

    let ssa_class_impl (_class : program_class_data) : method_ssa list =
        List.iteri (
            fun i (attr : attribute_data) ->
                StringTbl.add !symbol_table attr.name (Attribute i)
        ) _class.attributes;

        let ssa_ast_method (method_ : impl_method) : (ssa_id list * ssa_stmt list) =
            let _method = List.find (
                fun (data : impl_method) -> data.name = method_.name
            ) _class.methods in
            let local_counter = ref 0 in
            let temp_counter = ref 0 in

            match method_.body.data with
            | Internal id -> [], [{ id = Unit; _val = SSA_internal id }]
            | _ ->

            List.iteri (
                fun i formal ->
                    StringTbl.add !symbol_table formal (Parameter i)
            ) method_.formals;

            let cmds = ssa_gen_expr_body data _class.name method_.body symbol_table temp_counter local_counter in

            List.iter (
                fun formal ->
                    StringTbl.remove !symbol_table formal
            ) method_.formals;

            cmds
        in

        let methods = List.map (
            fun (method_ : impl_method) -> 
                let ids, cmds = ssa_ast_method method_ in
                {
                    class_name = _class.name;
                    method_name = method_name_gen _class.name method_.name;
                    arg_count = List.length method_.formals + 1;
                    
                    stmts = cmds;
                    ids = ids;
                }   
        ) _class.methods in

        List.iter (
            fun (method_ : impl_method) ->
                StringTbl.remove !symbol_table method_.name
        ) _class.methods;

        generate_constructor data _class :: methods
    in

    StringMap.fold (
        fun _ class_data acc ->
            let methods = ssa_class_impl class_data in
            acc @ methods
    ) data.data_map []