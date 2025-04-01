open D_tac_data
open H_asm_data

module StringMap = Map.Make(String)

type strlit_map = (string * string) list
type stack_map = int StringMap.t

type asm_data = {
    strlit_map : strlit_map;
    stack_map : stack_map;
}

let string_literal_id = ref 0

let generate_string_literal (s : string) : string = 
    let id = !string_literal_id in
    string_literal_id := id + 1;

    "str_" ^ (string_of_int id)

let generate_strlit_map (tac_cmds : tac_cmd list) : strlit_map =
    List.filter_map (fun cmd ->
        match cmd with
        | TAC_str (_, s) -> 
            let str_id = generate_string_literal s in
            Some (str_id, s)
        | _ -> None
    ) tac_cmds

let generate_stack_map (tac_ids : tac_id list) : stack_map =
    let rec generate_stack_map' (tac_ids : tac_id list) (stack_map : stack_map) (offset : int) =
        match tac_ids with
        | [] -> stack_map
        | id :: rest -> generate_stack_map' rest (StringMap.add id offset stack_map) (offset - 8)
    in

    generate_stack_map' tac_ids StringMap.empty (-8)

let generate_tac_asm (tac_cmd : tac_cmd) (asm_data : asm_data) : asm_cmd list = 
    let get_symbol_storage (id : tac_id) : asm_mem =
        match StringMap.find_opt id asm_data.stack_map with
        | Some offset -> RBP_offset offset
        | None -> failwith "ID not found in stack map"
    in
    
    match tac_cmd with
    | TAC_add (id, a, b) ->
        [
            MOV_reg ((get_symbol_storage a), RAX);
            MOV_reg ((get_symbol_storage b), RBX);
            ADD (REG RAX, RBX);
            MOV_mem (RBX, get_symbol_storage id)
        ]
    | TAC_sub (id, a, b) ->
        [
            MOV_reg ((get_symbol_storage a), RAX);
            MOV_reg ((get_symbol_storage b), RBX);
            SUB (REG RBX, RAX);
            MOV_mem (RAX, get_symbol_storage id)
        ]
    | TAC_mul (id, a, b) ->
        [
            MOV_reg ((get_symbol_storage a), RAX);
            MOV_reg ((get_symbol_storage b), RBX);
            MUL (REG RAX, RBX);
            MOV_mem (RBX, get_symbol_storage id)
        ]
    | TAC_div (id, a, b) ->
        [
            MOV_reg ((get_symbol_storage a), RAX);
            MOV_reg ((get_symbol_storage b), RBX);
            MISC "cqo";
            DIV RBX;
            MOV_mem (RAX, get_symbol_storage id)
        ]
    | TAC_lt (id, a, b) -> 
        [
            MOV_reg ((get_symbol_storage a), RBX);
            MOV_reg ((get_symbol_storage b), RCX);
            CMP (RCX, RBX);
            SETL;
            MOV_mem (RAX, get_symbol_storage id)
        ]
    | TAC_lte (id, a, b) -> 
        [
            MOV_reg ((get_symbol_storage a), RBX);
            MOV_reg ((get_symbol_storage b), RCX);
            XOR (RAX, RAX);
            CMP (RCX, RBX);
            SETLE;
            MOV_mem (RAX, get_symbol_storage id)
        ]
    | TAC_eq (id, a, b) ->
        [
            MOV_reg ((get_symbol_storage a), RBX);
            MOV_reg ((get_symbol_storage b), RCX);
            XOR (RAX, RAX);
            CMP (RBX, RCX);
            SETE;
            MOV_mem (RAX, get_symbol_storage id)
        ]
    | TAC_int (id, i) ->
        [
            MOV_reg (IMMEDIATE i, RAX);
            MOV_mem (RAX, get_symbol_storage id)
        ]
    | TAC_str (id, s) ->
        let str_id = fst @@ List.find (fun (_, _s) -> _s = s) asm_data.strlit_map in
        
        [
            MOV_reg (LABEL str_id, RAX);
            MOV_mem (RAX, get_symbol_storage id)
        ]
    | TAC_bool (id, b) ->
        let bool_val = if b then 1 else 0 in

        [
            MOV_reg (IMMEDIATE bool_val, RAX);
            MOV_mem (RAX, get_symbol_storage id)
        ]
    | TAC_ident (id, s) ->
        [
            MOV_reg ((get_symbol_storage s), RAX);
            MOV_mem (RAX, get_symbol_storage id)
        ]
    | TAC_neg (id, a) ->
        [
            MOV_reg ((get_symbol_storage a), RAX);
            NEG RAX;
            MOV_mem (RAX, get_symbol_storage id)
        ]
    | TAC_not (id, a) ->
        [
            MOV_reg ((get_symbol_storage a), RAX);
            TEST (RAX, RAX);
            SETE;
            MOV_mem (RAX, get_symbol_storage id)
        ]
    | TAC_call (id, method_name, args) ->
        let arg_cmds = List.concat @@ List.map (fun arg -> 
            let load = MOV_reg ((get_symbol_storage arg), RAX) in    
            let push = PUSH RAX in

            [load; push]
        ) @@ List.rev args in

        let pop_cmds = List.concat @@ List.map (fun _ -> [POP RAX]) args in

        arg_cmds @ [
            CALL method_name;
            MOV_mem (RAX, get_symbol_storage id)
        ] @ pop_cmds
    | TAC_default (id, s) ->
        [
            MOV_reg (IMMEDIATE 0, RAX);
            MOV_mem (RAX, get_symbol_storage id)
        ]

    | TAC_label label -> [LABEL label]
    | TAC_jmp label -> [JMP label]
    | TAC_bt (id, label) ->
        [
            MOV_reg ((get_symbol_storage id), RAX);
            TEST (RAX, RAX);
            JNZ label
        ]
    | TAC_return id ->
        [
            MOV_reg ((get_symbol_storage id), RAX);
            RET
        ]
    | TAC_comment s -> [COMMENT s]

    | TAC_new _ -> [COMMENT "New"]
    | TAC_isvoid _ -> [COMMENT "Isvoid"]


let generate_asm (method_tac : method_tac) : asm_method =
    let stack_space = 8 * (List.length method_tac.ids) in

    let asm_data = {
        strlit_map = generate_strlit_map method_tac.commands;
        stack_map = generate_stack_map method_tac.ids;
    } in

    let cmds = List.concat (List.map (fun (cmd : tac_cmd) -> generate_tac_asm cmd asm_data) method_tac.commands) in

    {
        header = method_tac.class_name ^ "_" ^ method_tac.method_name ^ "_0";
        arg_count = method_tac.arg_count;

        commands = (FRAME stack_space :: cmds);
        string_literals = asm_data.strlit_map;
    }