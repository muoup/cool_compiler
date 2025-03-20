open D_ast
open D_class_map
open D_impl_map
open E_parser_data
open G_tac_data
open I_tac_expr_gen

let generate_tac (data : parsed_data) : method_tac list =
    let symbol_table = ref @@ StringTbl.create 10 in
    StringTbl.add !symbol_table "self" "self";

    let add_symbol (x : string) : unit =
        StringTbl.add !symbol_table x x
    in

    let remove_symbol (x : string) : unit =
        StringTbl.remove !symbol_table x
    in

    let tac_ast_class (cls : ast_class) : method_tac list =
        let _class = List.find (fun (data : class_data) -> data.name = cls.name.name) data.class_map in
        let _class_impl = List.find (fun (data : impl_class) -> data.name = cls.name.name) data.impl_map in

        List.iter (fun (attr : attribute_data) -> add_symbol attr.name) _class.attributes;

        let tac_ast_method (method_ : impl_method) : tac_cmd list =
            let _method = List.find (fun (data : impl_method) -> data.name = method_.name) _class_impl.methods in

            (* Not sure how to replicate a Rust matches! macro, but this should work *)
            match method_.body.data with
            | Internal _ -> []
            | _ ->

            List.iter (add_symbol) method_.formals;

            let method_identifier = Printf.sprintf "%s_%s_0" cls.name.name method_.name in
            let cmds = tac_gen_expr_body data cls method_.body symbol_table in

            List.iter (remove_symbol) method_.formals;

            cmds
        in

        let methods = List.map (
            fun (method_ : impl_method) -> {
                class_name = cls.name.name;
                method_name = method_.name;
                commands = TAC_label "_entry" :: (tac_ast_method method_);
            }   
        ) _class_impl.methods in

        List.iter (fun (attr : attribute_data) -> remove_symbol attr.name) _class.attributes;

        methods
    in

    List.concat (List.map tac_ast_class data.ast)