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

(* This code should generate all error messages needed for any possible runtime error. 
Ideally in PA4, it's made slightly more intelligent than the reference compiler's "generate billions
and billions of error messages" strategy*)
let generate_error_messages(tac_cmds : tac_cmd list) : strlit_map = (
    List.filter_map (fun cmd ->
        match cmd with
        | TAC_div (result, nominator, denominator) -> 
            let div_msg = "ERROR: " ^ string_of_int(nominator.line_number) ^ ": Exception: division by zero" in
            let str_id = generate_string_literal result in
            Some (str_id, div_msg)
        | _ -> None
    ) tac_cmds
)

let generate_strlit_map (tac_cmds : tac_cmd list) : strlit_map =
    let error_msgs = generate_error_messages tac_cmds in
    let other_strings = 
    List.filter_map (fun cmd ->
        match cmd with
        | TAC_str (_, s) -> 
            let str_id = generate_string_literal () in
            Some (str_id, s)
        | _ -> None
    ) tac_cmds in
    error_msgs @ other_strings

let generate_stack_map (tac_ids : tac_id list) : stack_map =
    let rec generate_stack_map' (tac_ids : tac_id list) (stack_map : stack_map) (offset : int) =
        match tac_ids with
        | [] -> stack_map
        | id :: rest -> generate_stack_map' rest (StringMap.add id offset stack_map) (offset - 8)
    in

    generate_stack_map' tac_ids StringMap.empty (-8)

let get_label_from_string (str : string) (map : strlit_map) : string = 
    fst (List.find (fun s -> snd s = str) map)

let generate_tac_asm (tac_cmd : tac_cmd) (asm_data : asm_data) : asm_cmd list = 
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
        let div_msg = "ERROR: " ^ string_of_int(a.line_number) ^ ": Exception: division by zero" in
        let error_str = get_label_from_string div_msg asm_data.strlit_map in
        [
            COMMENT "Division by zero check";
            MOV_reg (IMMEDIATE line_number, RSI);
            MOV_reg32 ((get_symbol_storage b), RBX);
            TEST (RBX, RBX);
            JZ "error_div_zero";

            COMMENT "Division";
            MOV_reg32 ((get_symbol_storage a), RAX);
            (* MOV_reg32 ((get_symbol_storage a.id), RAX);
            MOV_reg32 ((get_symbol_storage b), RBX);
            MISC "cdq";
            DIV RBX;
            MOV_mem (RAX, get_symbol_storage id) *)
            COMMENT ("Check for division by 0");
            MOV_reg (IMMEDIATE 0, RBX);
            MOV_reg32 ((get_symbol_storage b), RBX);
            MOV_reg (IMMEDIATE 0, RAX);
            CMP (RAX, RBX);
            COMMENT error_str;
            JE ".error_out";
            COMMENT ("OK to divide");
            MOV_reg32 ((get_symbol_storage a.id), RAX);
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
        let dispatch_check = [
            COMMENT ("Dispatch on Void Check");
            MOV_reg (IMMEDIATE line_number, RSI);
            MOV_reg ((get_symbol_storage obj), RAX);
            TEST (RAX, RAX);
            JZ "error_dispatch";
        ] in
        
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

        dispatch_check @ arg_cmds @ load_vtable @ pop_cmds

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
            MOV_reg     (IMMEDIATE 8, RDI);
            MOV_reg     (IMMEDIATE size, RSI);
            XOR         (RAX, RAX);
            CALL        "calloc";

            MOV_mem     (RAX, get_symbol_storage id);

            MOV_reg     (LABEL (obj_name_mem_gen object_name), RDI);
            MOV_mem     (RDI, REG_offset (RAX, 0));

            MOV_reg     (IMMEDIATE size, RDI);
            MOV_mem     (RDI, REG_offset (RAX, 8));

            MOV_reg     (LABEL (vtable_name_gen object_name), RDI);
            MOV_mem     (RDI, REG_offset (RAX, 16))
        ]

    | TAC_internal id -> generate_internal_asm id

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
        header = method_tac.method_name;
        arg_count = method_tac.arg_count;

        commands = (FRAME stack_space :: cmds);
        string_literals = asm_data.strlit_map;
    }