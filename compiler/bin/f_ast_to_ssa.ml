open A_util
open B_ast
open B_class_map
open B_impl_map
open C_parser_data
open D_ssa_data
open E_expr_to_ssa

let generate_ssa (data : program_data) : method_ssa list =
    let symbol_table : ssa_sym_table ref = ref @@ StringTbl.create 10 in
    let id_count : int ref = ref 0 in

    StringTbl.add !symbol_table "self" { id = SSA_self; _type = "self" };

    let gen_method_ssa (_method : impl_method) : method_ssa =
        match _method.body.data with
        | Internal id ->
            {
                class_name = _method.parent;
                method_name = _method.name;
                arg_count = List.length _method.formals;

                blocks = [
                    {
                        label = "entry";
                        stmts = [
                            SSA_Valueless (SSA_internal id);
                        ]
                    };
                ]
            }  
        | _ ->

        let return_type, _ = get_method_signature data _method.parent _method.name in

        let body = ssa_from_expr
            data _method.parent return_type 
            _method.body id_count symbol_table in

        {
            class_name = _method.parent;
            method_name = _method.name;
            arg_count = List.length _method.formals;

            blocks = body.blocks @ 
                [
                    {
                        label = label_id () ^ "_exit";
                        stmts = [
                            SSA_Valueless (SSA_return body.end_val);
                        ]
                    }
                ]
        }
    in

    data.data_map
    |> StringMap.bindings
    |> List.map (
        fun (_, _class) ->
            List.map (gen_method_ssa) _class.methods
        )
    |> List.flatten