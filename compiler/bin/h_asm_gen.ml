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

let generate_stack_map (_method : method_tac) : stack_map =
    let local_variable_offset = 0 in
    let temporaries_offset = local_variable_offset + _method.locals * 8 in

    {
        local_variable_offset = local_variable_offset;
        temporaries_offset = temporaries_offset;
    }

let get_parameter_memory (i : int) : asm_mem =
    REG_offset (RBP, 24 + (i * 8))

let get_id_memory (id : tac_id) (stack_map : stack_map) : asm_mem =
    match id with
    | Local     i -> REG_offset (RBP, -stack_map.local_variable_offset - ((i + 1) * 8))
    | Temporary i -> REG_offset (RBP, -stack_map.temporaries_offset - ((i + 1) * 8))
    | Attribute i -> REG_offset (R12, 24 + 8 * i)
    | CallSlot  i -> REG_offset (RSP, 8 * i)
    | Parameter i -> get_parameter_memory i
    | Self        -> REG R12
    | IntLit    i -> IMMEDIATE i
    | StrLit    s -> LABEL s 

let generate_internal_asm (class_name : string) (internal_id : string) : asm_cmd list =
    match internal_id with
    | "IO.in_string" ->
        [
            CALL    "in_string";
            RET;
        ]
    | "IO.out_string" ->
        [
            PUSH    (REG RAX);
            PUSH    (get_parameter_memory 0);
            CALL    "out_string";
            ADD     (IMMEDIATE 16, RSP);
            MOV     (REG R12, REG RAX);
            RET
        ]
    | "IO.in_int" ->
        [
            CALL    "in_int";
            RET;
        ]
    | "IO.out_int" ->
        [
            PUSH    (REG RAX);
            PUSH    (get_parameter_memory 0);
            CALL    "out_int";
            ADD     (IMMEDIATE 16, RSP);
            MOV     (REG R12, REG RAX);
            RET
        ]
    | "Object.type_name" ->
        [
            MOV     (LABEL (obj_name_mem_gen class_name), REG RAX);
            RET;
        ]
    | "Object.abort" ->
        [
            MOV     (LABEL "abort_msg", REG RDI);
            CALL    "puts";

            MOV     (IMMEDIATE 1, REG RDI);
            CALL    "exit";
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
            MOV     (REG R12, REG RDI);
            CALL    "strlen";
            RET;
        ]
    | x -> 
        [
            COMMENT ("Unimplemented: " ^ x);
            CALL    "exit";
        ]

let generate_tac_asm (tac_cmd : tac_cmd) (current_class : string) (asm_data : asm_data) : asm_cmd list = 
    let get_symbol_storage (id : tac_id) : asm_mem =
        get_id_memory id asm_data.stack_map
    in
    
    match tac_cmd with
    | TAC_add (id, a, b) ->
        [
            MOV     ((get_symbol_storage a), REG RAX);
            ADD     ((get_symbol_storage b), RAX);
            MOV     (REG RAX, get_symbol_storage id)
        ]
    | TAC_sub (id, a, b) ->
        [
            MOV     ((get_symbol_storage a), REG RAX);
            SUB     ((get_symbol_storage b), RAX);
            MOV     (REG RAX, get_symbol_storage id)
        ]
    | TAC_mul (id, a, b) ->
        [
            MOV     ((get_symbol_storage a), REG RAX);
            MUL     ((get_symbol_storage b), RAX);
            MOV     (REG RAX, get_symbol_storage id)
        ]
    | TAC_div (line_number, id, a, b) ->
        [
            COMMENT "Division by zero check";
            MOV     (IMMEDIATE line_number, REG RSI);
            MOV32   ((get_symbol_storage b), REG RBX);
            TEST    (REG RBX, REG RBX);
            JZ      "error_div_zero";

            COMMENT "Division";
            MOV32   ((get_symbol_storage a), REG RAX);
            MISC    "cdq";
            DIV     RBX;
            MOV     (REG RAX, get_symbol_storage id)
        ]
    | TAC_lt (id, a, b) -> 
        [
            CMP     (get_symbol_storage b, get_symbol_storage a);
            MOV     (IMMEDIATE 0, REG RAX);
            SETL;
            MOV     (REG RAX, get_symbol_storage id)
        ]
    | TAC_lte (id, a, b) -> 
        [
            CMP     (get_symbol_storage b, get_symbol_storage a);
            MOV     (IMMEDIATE 0, REG RAX);
            SETLE;
            MOV     (REG RAX, get_symbol_storage id)
        ]
    | TAC_eq (id, a, b) ->
        [
            CMP     (get_symbol_storage b, get_symbol_storage a);
            MOV     (IMMEDIATE 0, REG RAX);
            SETE;
            MOV     (REG RAX, get_symbol_storage id)
        ]
    | TAC_int (id, i) ->
        [
            MOV     (IMMEDIATE i, get_symbol_storage id);
        ]
    | TAC_str (id, s) ->
        let str_id = fst @@ List.find (fun (_, _s) -> _s = s) asm_data.strlit_map in
        
        [
            MOV     (LABEL str_id, get_symbol_storage id);
        ]
    | TAC_bool (id, b) ->
        let bool_val = if b then 1 else 0 in

        [
            MOV     (IMMEDIATE bool_val, get_symbol_storage id);
        ]
    | TAC_ident (id, s) ->
        [
            MOV     (get_symbol_storage s, get_symbol_storage id)
        ]
    | TAC_neg (id, a) ->
        [
            MOV     (get_symbol_storage a, get_symbol_storage id);
            NEG     (get_symbol_storage id);
        ]
    | TAC_not (id, a) ->
        [
            MOV     (get_symbol_storage a, REG RAX);
            TEST    (REG RAX, REG RAX);
            MOV     (IMMEDIATE 0, REG RAX);
            SETE;
            MOV     (REG RAX, get_symbol_storage id)
        ]
    | TAC_call_alloc slots ->
        let slots = if (slots mod 2 = 0) then slots else slots + 1 in

        [
            SUB         (IMMEDIATE (8 * slots), RSP);
        ]
    | TAC_call (id, method_name, args) ->
        let slots = List.length args in
        let slots = if (slots mod 2 = 0) then slots else slots + 1 in

        [
            CALL method_name;
            ADD (IMMEDIATE (8 * slots), RSP);
            MOV (REG RAX, get_symbol_storage id)
        ]

    | TAC_dispatch { line_number; store; obj; method_id; args } ->
        let slots = List.length args in
        let slots = if (slots mod 2 = 0) then slots else slots + 1 in

        [
            COMMENT ("Load Vtable ID " ^ (string_of_int method_id));
            MOV     (get_symbol_storage obj, REG RAX);
            MOV     (REG_offset (RAX, 16), REG RAX);
            MOV     (REG_offset (RAX, 8 * method_id), REG RAX);
            CALL_indirect   RAX;
            ADD     (IMMEDIATE (8 * slots), RSP);
            MOV     (REG RAX, get_symbol_storage store)
        ]

    | TAC_str_eq  (id, s1, s2) ->
        [
            MOV     ((get_symbol_storage s1), REG RDI);
            MOV     ((get_symbol_storage s2), REG RSI);
            CALL    "strcmp";
            TEST    (REG RAX, REG RAX);
            MOV     (IMMEDIATE 0, REG RAX);    
            SETE;
            MOV     (REG RAX, get_symbol_storage id)
        ]
    | TAC_str_lt (id, s1, s2) ->
        [
            MOV     ((get_symbol_storage s1), REG RDI);
            MOV     ((get_symbol_storage s2), REG RSI);
            CALL    "strcmp";
            TEST    (REG RAX, REG RAX);
            MOV     (IMMEDIATE 0, REG RAX);
            SETL;
            MOV     (REG RAX, get_symbol_storage id)
        ]
    | TAC_str_lte (id, s1, s2) ->
        [
            MOV     ((get_symbol_storage s1), REG RDI);
            MOV     ((get_symbol_storage s2), REG RSI);
            CALL    "strcmp";
            TEST    (REG RAX, REG RAX);
            MOV     (IMMEDIATE 0, REG RAX);
            SETLE;
            MOV     (REG RAX, get_symbol_storage id)
        ]

    | TAC_default (id, _type) ->
        (match _type with
        | "String" ->
            [
                MOV     (LABEL "default_string", get_symbol_storage id);
            ]
        | _ ->
            [
                MOV     (IMMEDIATE 0, get_symbol_storage id);
            ])

    | TAC_label label -> [LABEL label]
    | TAC_jmp label -> [JMP label]
    | TAC_bt (id, label) ->
        [
            TEST        (get_symbol_storage id, get_symbol_storage id);
            JNZ         label
        ]
    | TAC_return id ->
        [
            MOV         (get_symbol_storage id, REG RAX);
            RET
        ]
    | TAC_comment s -> [COMMENT s]
    | TAC_new (id, name) ->
        [
            CALL        (constructor_name_gen name);
            MOV         (REG RAX, (get_symbol_storage id))
        ]

    (* Object creation *)

    | TAC_object (id, object_name, attributes) ->
        let size = 8 * (3 + attributes) in

        [
            MOV         (IMMEDIATE 1, REG RDI);
            MOV         (IMMEDIATE size, REG RSI);
            XOR         (RAX, RAX);
            CALL        "calloc";

            MOV         (REG RAX, REG R12);
            MOV         (REG RAX, get_symbol_storage id);

            MOV         (LABEL (obj_name_mem_gen object_name), REG_offset (RAX, 0));
            MOV         (IMMEDIATE size, REG_offset (RAX, 8));
            MOV         (LABEL (vtable_name_gen object_name), REG_offset (RAX, 16))
        ]

    | TAC_inline_assembly asm_code ->
        [
            COMMENT     ("Inline assembly: " ^ asm_code);
            MISC        asm_code
        ]
    | TAC_internal id -> generate_internal_asm current_class id

    | TAC_isvoid (store, _val) -> 
        [
            TEST        (get_symbol_storage _val, get_symbol_storage _val);
            MOV         (IMMEDIATE 0, REG RAX);
            SETE;
            MOV         (REG RAX, get_symbol_storage store);
        ]

    | TAC_void_check (line_number, object_id, error) ->
        [
            MOV         (IMMEDIATE line_number, REG RSI);
            TEST        (get_symbol_storage object_id, get_symbol_storage object_id);
            JE          error
        ]

let generate_asm (method_tac : method_tac) : asm_method =
    let asm_data = {
        strlit_map = generate_strlit_map method_tac.commands;
        stack_map = generate_stack_map method_tac;
    } in

    let stack_space = (method_tac.locals + method_tac.temps + 1) * 8 in

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