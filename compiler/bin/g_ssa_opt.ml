open A_util
open D_ssa_data

let upgrade_const_vars (_method : method_ssa) : method_ssa =
    let assignments = List.filter_map (
        fun stmt -> match stmt._val with
            | SSA_store _ -> Some stmt.id
            | _ -> None
    ) _method.stmts in

    let mutated = StringSet.of_list @@ List.map (f_id) assignments in

    let upgrade_val (stmt : ssa_stmt) : ssa_stmt =
        if StringSet.mem (f_id stmt.id) mutated then stmt
        else

        { stmt with _val = 
            match stmt._val with
            | SSA_default_mem type_name -> SSA_default type_name
            | SSA_valued_mem (value, type_name) -> SSA_ident value
            | SSA_load value -> SSA_ident value
            | _ -> stmt._val
        }
    in

    let upgraded = List.map (upgrade_val) _method.stmts in

    { _method with stmts = upgraded }

let constant_fold (_method : method_ssa) : method_ssa =
    let ident_map : ssa_id StringMap.t ref = ref StringMap.empty in
    
    let add_ident (id : ssa_id) (value : ssa_id) : unit =
        match id with
        | Local i -> ident_map := StringMap.add (f_id id) value !ident_map
        | _ -> ()
    in

    let rec inline_ident (id : ssa_id) : ssa_id =
        match id with
        | Local _ -> 
            begin match StringMap.find_opt (f_id id) !ident_map with
            | Some value ->
                begin match value with
                | Local i -> Local i
                | _ -> value
                end
            | None -> id
            end
        | _ -> id
    in

    let rec inline_int_binop (stmt : ssa_stmt) (lhs : ssa_id) (rhs : ssa_id) (op : int -> int -> int) : ssa_val =
        let lhs = inline_ident lhs in
        let rhs = inline_ident rhs in

        match lhs, rhs with
        | IntLiteral lhs, IntLiteral rhs ->
            let _val = op lhs rhs in
            ident_map := StringMap.add (f_id stmt.id) (IntLiteral _val) !ident_map;
            
            SSA_ident (IntLiteral _val)
        | _ -> stmt._val
    in
    
    let fold_stmt (stmt : ssa_stmt) : ssa_stmt =
        { stmt with _val = 
            match stmt._val with
            | SSA_ident value ->
                let value = inline_ident value in
                add_ident stmt.id value;
                SSA_ident value
            | SSA_default type_name ->
                begin match type_name with
                | "Int" ->
                    add_ident stmt.id (IntLiteral 0);
                    SSA_ident (IntLiteral 0)
                | "String" ->
                    add_ident stmt.id (StringLiteral "");
                    SSA_ident (StringLiteral "")
                | "Bool" ->
                    add_ident stmt.id (BoolLiteral false);
                    SSA_ident (BoolLiteral false)
                | _ -> stmt._val
                end;
            | SSA_add (lhs, rhs) -> inline_int_binop stmt lhs rhs ( + )
            | SSA_sub (lhs, rhs) -> inline_int_binop stmt lhs rhs ( - )
            | SSA_mul (lhs, rhs) -> inline_int_binop stmt lhs rhs ( * )
            | SSA_div (_, lhs, rhs) -> inline_int_binop stmt lhs rhs ( / )

            | SSA_lt  (lhs, rhs) -> inline_int_binop stmt lhs rhs (fun x y -> if x < y then 1 else 0)
            | SSA_lte (lhs, rhs) -> inline_int_binop stmt lhs rhs (fun x y -> if x <= y then 1 else 0)
            | SSA_eq  (lhs, rhs) -> inline_int_binop stmt lhs rhs (fun x y -> if x = y then 1 else 0)

            | SSA_call (method_name, args) ->
                let args = List.map (inline_ident) args in
                SSA_call (method_name, args)

            | SSA_dispatch dispatch ->
                let args = List.map (inline_ident) dispatch.args in
                SSA_dispatch { dispatch with args = args }

            | SSA_return id -> SSA_return (inline_ident id)

            | _ -> stmt._val
        }
    in

    let stmts = List.map (fold_stmt) _method.stmts in

    { _method with stmts = stmts }

let remove_aliases (_method : method_ssa) : method_ssa =
    let stmts = List.filter (
        fun stmt ->
            match stmt._val with
            | SSA_ident _ -> false
            | _ -> true
    ) _method.stmts in

    { _method with stmts = stmts }

let optimize_ssa (ssa : method_ssa) : method_ssa =
    (* remove_aliases @@ *) (* This is just waiting to cause bugs for no reason, but useful for debugging *)
    constant_fold @@
    upgrade_const_vars @@
    ssa