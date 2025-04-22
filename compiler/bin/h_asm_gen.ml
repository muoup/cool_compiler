open A_util
open D_tac_data
open H_asm_data

type strlit_map = (string * string) list
type stack_map = {
    local_variable_offset : int;
    temporaries_offset : int;
}

type asm_data = {
    strlit_map : strlit_map;
    stack_map : stack_map;
}

let string_literal_id = ref 0

let generate_string_literal () : string = 
    let id = !string_literal_id in
    string_literal_id := id + 1;

    "str_" ^ (string_of_int id)

let generate_strlit_map (tac_cmds : tac_cmd list) : strlit_map =
    List.filter_map (fun cmd ->
        match cmd with
        | TAC_str (_, s) -> 
            let str_id = generate_string_literal () in
            Some (str_id, s)
        | _ -> None
    ) tac_cmds

let generate_stack_map (tac_ids : tac_id list) : stack_map =
    let (locals, temps) = List.fold_left (
        fun (acc_locals, acc_temps) id ->
            match id with
            | Local _ -> (acc_locals + 1, acc_temps)
            | Temporary _ -> (acc_locals, acc_temps + 1)
            | _ -> (acc_locals, acc_temps)
    ) (0, 0) tac_ids in

    let local_variable_offset = 0 in
    let temporaries_offset = local_variable_offset + (locals * 8) in

    {
        local_variable_offset = local_variable_offset;
        temporaries_offset = temporaries_offset;
    }

let get_parameter_memory (i : int) : asm_mem =
    RBP_offset (24 + (i * 8))

let get_id_memory (id : tac_id) (stack_map : stack_map) : asm_mem =
    match id with
    | Local     i -> RBP_offset (-stack_map.local_variable_offset - ((i + 1) * 8))
    | Temporary i -> RBP_offset (-stack_map.temporaries_offset - ((i + 1) * 8))
    | Attribute i -> REG_offset (R12, 24 + 8 * i)
    | Self        -> REG R12
    | Parameter i -> get_parameter_memory i

let generate_internal_asm (class_name : string) (internal_id : string) : asm_cmd list =
    match internal_id with
    | "IO.in_string" ->
        [
            CALL "in_string";
            RET
        ]
    | "IO.out_string" ->
        [
            MOV_reg (get_parameter_memory 0, RAX);
            PUSH RAX;
            CALL "out_string";
            POP RAX;
            MOV_reg (REG R12, RAX);
            RET
        ]
    | "IO.in_int" ->
        [
            CALL "in_int";
            RET
        ]
    | "IO.out_int" ->
        [
            MOV_reg (get_parameter_memory 0, RAX);
            PUSH RAX;
            CALL "out_int";
            POP RAX;
            MOV_reg (REG R12, RAX);
            RET
        ]
    | "Object.type_name" ->
        [
            MOV_reg (LABEL (obj_name_mem_gen class_name), RAX);
            RET
        ]
    | "Object.abort" ->
        [
            MOV_reg (LABEL "abort_msg", RDI);
            CALL "puts";

            MOV_reg (IMMEDIATE 1, RDI);
            CALL "exit";
        ]
    | "Object.copy" ->
        [
            JMP "copy";
        ]
    | "String.concat" ->
        [
            JMP "concat";
        ]
    | "String.substr" ->
        [
            JMP "substr";
        ]
    | "String.length" ->
        [
            MOV_reg (REG R12, RDI);
            CALL "strlen";
            RET;
        ]
    | x -> 
        [
            COMMENT ("Unimplemented: " ^ x);
            CALL "exit";
        ]

let generate_tac_asm (tac_cmd : tac_cmd) (current_class : string) (asm_data : asm_data) : asm_cmd list = 
    let get_symbol_storage (id : tac_id) : asm_mem =
        get_id_memory id asm_data.stack_map
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
    | TAC_div (line_number, id, a, b) ->
        [
            COMMENT "Division by zero check";
            MOV_reg (IMMEDIATE line_number, RSI);
            MOV_reg32 ((get_symbol_storage b), RBX);
            TEST (RBX, RBX);
            JZ "error_div_zero";

            COMMENT "Division";
            MOV_reg32 ((get_symbol_storage a), RAX);
            MISC "cdq";
            DIV RBX;
            MOV_mem (RAX, get_symbol_storage id)

        ]
    | TAC_lt (id, a, b) -> 
        [
            MOV_reg ((get_symbol_storage a), RBX);
            MOV_reg ((get_symbol_storage b), RCX);
            XOR (RAX, RAX);
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

        let pop_cmds = [ADD (IMMEDIATE (8 * (List.length args)), RSP)] in

        arg_cmds @ [
            CALL method_name;
            MOV_mem (RAX, get_symbol_storage id)
        ] @ pop_cmds

    | TAC_dispatch { line_number; store; obj; method_id; args } ->
        let load_vtable = [
            COMMENT ("Load Vtable ID " ^ (string_of_int method_id));
            MOV_reg (get_symbol_storage obj, RAX);
            MOV_reg (REG_offset (RAX, 16), RAX);
            MOV_reg (REG_offset (RAX, 8 * method_id), RAX);
            CALL_indirect RAX;
            MOV_mem (RAX, get_symbol_storage store)
        ] in

        let arg_cmds = List.concat @@ List.map (fun arg -> 
            let load = MOV_reg ((get_symbol_storage arg), RAX) in    
            let push = PUSH RAX in

            [load; push]
        ) @@ List.rev args in

        let pop_cmds = [ADD (IMMEDIATE (8 * (List.length args)), RSP)] in

        arg_cmds @ load_vtable @ pop_cmds

    | TAC_str_eq  (id, s1, s2) ->
        [
            MOV_reg ((get_symbol_storage s1), RDI);
            MOV_reg ((get_symbol_storage s2), RSI);
            CALL "strcmp";
            MOV_reg (REG RAX, RDI);
            XOR (RAX, RAX);
            TEST (RDI, RDI);
            SETE;
            MOV_mem (RAX, get_symbol_storage id)
        ]
    | TAC_str_lt (id, s1, s2) ->
        [
            MOV_reg ((get_symbol_storage s1), RDI);
            MOV_reg ((get_symbol_storage s2), RSI);
            CALL "strcmp";
            MOV_reg (REG RAX, RDI);
            XOR (RAX, RAX);
            CMP (RAX, RDI);
            SETL;
            MOV_mem (RAX, get_symbol_storage id)
        ]
    | TAC_str_lte (id, s1, s2) ->
        [
            MOV_reg ((get_symbol_storage s1), RDI);
            MOV_reg ((get_symbol_storage s2), RSI);
            CALL "strcmp";
            MOV_reg (REG RAX, RDI);
            XOR (RAX, RAX);
            CMP (RAX, RDI);
            SETLE;
            MOV_mem (RAX, get_symbol_storage id)
        ]

    | TAC_default (id, _type) ->
        (match _type with
        | "String" ->
            [
                MOV_reg (LABEL "default_string", RAX);
                MOV_mem (RAX, get_symbol_storage id)
            ]
        | _ ->
            [
                MOV_reg (IMMEDIATE 0, RAX);
                MOV_mem (RAX, get_symbol_storage id)
            ])

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
    | TAC_new (id, name) ->
        [
            CALL (constructor_name_gen name);
            MOV_mem (RAX, (get_symbol_storage id))
        ]

    | TAC_attribute { object_id; attribute_id; value } ->
        [
            MOV_reg ((get_symbol_storage object_id), RAX);
            MOV_reg ((get_symbol_storage value), RBX);
            MOV_mem (RBX, REG_offset (RAX, 24 + 8 * attribute_id))
        ]

    (* Object creation *)

    | TAC_object (id, object_name, attributes) ->
        let size = 8 * (3 + attributes) in

        [
            MOV_reg     (IMMEDIATE 1, RDI);
            MOV_reg     (IMMEDIATE size, RSI);
            XOR         (RAX, RAX);
            CALL        "calloc";

            MOV_mem     (RAX, REG R12);
            MOV_mem     (RAX, get_symbol_storage id);

            MOV_reg     (LABEL (obj_name_mem_gen object_name), RDI);
            MOV_mem     (RDI, REG_offset (RAX, 0));

            MOV_reg     (IMMEDIATE size, RDI);
            MOV_mem     (RDI, REG_offset (RAX, 8));

            MOV_reg     (LABEL (vtable_name_gen object_name), RDI);
            MOV_mem     (RDI, REG_offset (RAX, 16))
        ]

    | TAC_inline_assembly asm_code ->
        [
            COMMENT ("Inline assembly: " ^ asm_code);
            MISC asm_code
        ]
    | TAC_internal id -> generate_internal_asm current_class id

    | TAC_isvoid (store, _val) -> 
        [
            MOV_reg     (get_symbol_storage _val, RBX);
            XOR         (RAX, RAX);
            TEST        (RBX, RBX);
            SETE;
            MOV_mem     (RAX, get_symbol_storage store);
        ]

    | TAC_void_check (line_number, object_id, error) ->
        [
            MOV_reg     (IMMEDIATE line_number, RSI);
            MOV_reg     (get_symbol_storage object_id, RBX);
            XOR         (RAX, RAX);
            TEST        (RBX, RBX);
            JE          error
        ]

let generate_asm (method_tac : method_tac) : asm_method =
    let stack_space = 8 * (List.length method_tac.ids) in
    let asm_data = {
        strlit_map = generate_strlit_map method_tac.commands;
        stack_map = generate_stack_map method_tac.ids;
    } in

    let cmds = 
        List.concat @@ 
        List.map (
            fun (cmd : tac_cmd) -> generate_tac_asm cmd method_tac.class_name asm_data
        ) method_tac.commands
    in

    {
        class_name = method_tac.class_name;
        header = method_tac.method_name;
        arg_count = method_tac.arg_count;

        commands = (FRAME stack_space :: cmds);
        string_literals = asm_data.strlit_map;
    }