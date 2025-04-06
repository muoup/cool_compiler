open A_util
open D_ssa_data
open D_tac_data

type ssa_map = ssa_id StringMap.t

type lifetime_ssa = {
    stmt : ssa_stmt;
    lifetime_ends : int list;
}

let get_ssa_stmt (id : ssa_id) (ssa : method_ssa) : ssa_stmt =
    let rec find_stmt id stmts =
        match stmts with
        | [] -> failwith "SSA statement not found"
        | stmt :: rest ->
            if id = stmt.id then stmt
            else find_stmt id rest
    in
    find_stmt id ssa.stmts

let has_side_affect (ssa : ssa_stmt) : bool =
    match ssa._val with
    | SSA_add _ | SSA_sub _ | SSA_mul _ | SSA_div _
    | SSA_lt _ | SSA_lte _ | SSA_eq _ -> false

    | _ -> true

let rec stmt_to_tac data (_method : method_ssa) (ssa : ssa_stmt) : tac_id * (tac_cmd list) =
    let id_map, temp_storage = data in
    
    let rec get_or_gen (id : ssa_id) =
        match StringMap.find_opt (f_id id) !id_map with
        | Some (Local i) -> get_or_gen (Local i)
        | Some id -> id
        | None ->
            let stmt = List.find (fun s -> s.id = id) _method.stmts in
            let new_id = stmt_to_tac data _method stmt in
            id_map := StringMap.add id new_id !id_map;
            new_id
    in

    let gen_id () =
        let rec gen_id' l i =
            match l with
            | [false] -> i
            | true :: rest -> gen_id' rest (i + 1)
            | [] -> temp_storage := !temp_storage @ [true]; i
        in

        gen_id' !temp_storage 0
    in

    let id = gen_id () in

    match ssa._val with
    | SSA_ident ident ->
        let ident = get_or_gen ident in
        
        match ident with
        | Local i -> (id, [TAC_load i])

        | IntLiteral i -> (id, [TAC_int i])
        | StringLiteral s -> (id, [TAC_string s])
        | BoolLiteral b -> (id, [TAC_bool b])

        | _ -> failwith "Invalid SSA identifier"
    | SSA_dispatch { line_number; object_id; method_name; args } ->
        let object_id = get_or_gen object_id in
        let args = List.map (get_or_gen) args in
        
        id, [TAC_dispatch { line_number; store = id; obj = object_id; method_id = 0; args }]
    | _ -> failwith "Unsupported SSA statement"
        

let ssa_to_tac (ssa : method_ssa) : tac_method =
    let side_affect_stmts = List.filter has_side_affect ssa.stmts in
    let id_map : (tac_id StringMap.t ref) = ref (StringMap.empty) in
    let temp_storage : (ref bool list) = [] in
    
    List.map (snd @@ stmt_to_tac id_map) side_affect_stmts
    |> List.flatten