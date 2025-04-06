open A_util
open D_ssa_data

let upgrade_const_vars (_method : method_ssa) : method_ssa =
    let assignments = List.filter_map (
        fun stmt -> match stmt with
            | SSA_store (id, _) -> Some id
            | _ -> None
    ) _method.stmts in

    let mutated = StringSet.of_list @@ List.map (f_id) assignments in

    let upgrade_stmt (stmt : ssa_stmt) : ssa_stmt =
        match stmt with
        | SSA_default_mem (id, type_name) ->
            if not @@ StringSet.mem (f_id id) mutated then
                SSA_default (id, type_name)
            else
                stmt
        | SSA_valued_mem (id, value, type_name) ->
            if not @@ StringSet.mem (f_id id) mutated then
                SSA_default (id, type_name)
            else
                stmt
        | SSA_load (id, value) ->
            if not @@ StringSet.mem (f_id id) mutated then
                SSA_ident (id, value)
            else
                stmt
        | _ -> stmt
    in

    let upgraded = List.map (upgrade_stmt) _method.stmts in

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

    let rec inline_int_binop (stmt : ssa_stmt) (id : ssa_id) (lhs : ssa_id) (rhs : ssa_id) (op : int -> int -> int) : ssa_stmt =
        print_ssa_stmt (Printf.printf "Inlining: %s\n") stmt;

        let lhs = inline_ident lhs in
        let rhs = inline_ident rhs in

        match lhs, rhs with
        | IntLiteral lhs, IntLiteral rhs ->
            let _val = op lhs rhs in
            ident_map := StringMap.add (f_id id) (IntLiteral _val) !ident_map;
            
            SSA_ident (id, IntLiteral _val)
        | _ ->
            Printf.printf "Not inlining: (%s, %s)\n" (f_id lhs) (f_id rhs);
            stmt
    in
    
    let fold_stmt (stmt : ssa_stmt) : ssa_stmt =
        match stmt with
        | SSA_ident (id, value) ->
            let value = inline_ident value in
            add_ident id value;
            SSA_ident (id, value)
        | SSA_default (id, type_name) ->
            begin match type_name with
            | "Int" ->
                add_ident id (IntLiteral 0);
                SSA_ident (id, IntLiteral 0)
            | "String" ->
                add_ident id (StringLiteral "");
                SSA_ident (id, StringLiteral "")
            | "Bool" ->
                add_ident id (BoolLiteral false);
                SSA_ident (id, BoolLiteral false)
            | _ -> stmt
            end;
        | SSA_add (id, lhs, rhs) -> inline_int_binop stmt id lhs rhs ( + )
        | SSA_sub (id, lhs, rhs) -> inline_int_binop stmt id lhs rhs ( - )
        | SSA_mul (id, lhs, rhs) -> inline_int_binop stmt id lhs rhs ( * )
        | SSA_div (_, id, lhs, rhs) -> inline_int_binop stmt id lhs rhs ( / )

        | SSA_lt  (id, lhs, rhs) -> inline_int_binop stmt id lhs rhs (fun x y -> if x < y then 1 else 0)
        | SSA_lte (id, lhs, rhs) -> inline_int_binop stmt id lhs rhs (fun x y -> if x <= y then 1 else 0)
        | SSA_eq  (id, lhs, rhs) -> inline_int_binop stmt id lhs rhs (fun x y -> if x = y then 1 else 0)

        | SSA_call (id, method_name, args) ->
            let args = List.map (inline_ident) args in
            SSA_call (id, method_name, args)

        | SSA_dispatch dispatch ->
            let args = List.map (inline_ident) dispatch.args in
            SSA_dispatch { dispatch with args = args }

        | SSA_return id -> SSA_return (inline_ident id)

        | _ -> stmt
    in

    let stmts = List.map fold_stmt _method.stmts in

    { _method with stmts = stmts }

let dead_code_elim (_method : method_ssa) : method_ssa =
    let stmts = List.filter (
        fun stmt ->
            match stmt with
            | SSA_ident (_, _) -> false
            | _ -> true
    ) _method.stmts in

    { _method with stmts = stmts }

let optimize_ssa (ssa : method_ssa) : method_ssa =
    (* dead_code_elim @@ *) (* This is just waiting to cause bugs for no reason, but useful for debugging *)
    constant_fold @@
    upgrade_const_vars @@
    ssa