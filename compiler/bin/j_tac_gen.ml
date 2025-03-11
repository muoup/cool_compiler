open D_ast
open D_class_map
open D_impl_map
open G_tac_data
open I_tac_expr_gen

let generate_tac (class_map : class_data list) (impl_map : impl_class list) (ast : ast) : tac_cmd list =
    let tac_ast_class (cls : ast_class) : tac_cmd list =
        let _class = List.find (fun (data : class_data) -> data.name = cls.name.name) class_map in
        let params = List.map (
            fun (x : ast_attribute) -> 
                match x with
                | AttributeNoInit { name } -> name.name
                | AttributeInit { name } -> name.name
        ) _class.attributes in

        let tac_ast_method (method_ : ast_method) : tac_cmd list =
            let label = Printf.sprintf "%s_%s_0" cls.name.name method_.name.name in
            TAC_label label :: tac_gen_expr_body method_.body params
        in

        List.concat (List.map tac_ast_method cls.methods)
    in

    List.concat (List.map tac_ast_class ast)