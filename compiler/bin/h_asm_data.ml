type asm_reg =
    | RAX   | RBX   | RCX   | RDX
    | RSI   | RDI   | RBP   | RSP
    | R8    | R9    | R10   | R11
    | R12

type asm_mem = 
    | RBP_offset    of int
    | REG           of asm_reg
    | LABEL         of string
    | IMMEDIATE     of int

type asm_cmd =
    | FRAME     of int 

    | MOV_reg   of asm_mem * asm_reg
    | MOV_mem   of asm_reg * asm_mem

    | ADD       of asm_mem * asm_reg
    | SUB       of asm_mem * asm_reg
    | MUL       of asm_mem * asm_reg
    | DIV       of asm_reg
    | XOR       of asm_reg * asm_reg

    | NEG       of asm_reg
    | NOT       of asm_reg

    | TEST      of asm_reg * asm_reg
    | CMP       of asm_reg * asm_reg
    | SETL
    | SETLE
    | SETE


    | PUSH      of asm_reg
    | POP       of asm_reg

    | CALL      of string
    | JMP       of string
    | JNZ       of string
    | JE        of string
    | RET

    | MISC      of string

    | LABEL     of string

    | COMMENT   of string

type asm_method = {
    header: string;
    arg_count: int;

    commands: asm_cmd list;
    string_literals: (string * string) list;
}

let asm_reg_to_string (reg : asm_reg) : string =
    match reg with
    | RAX -> "%rax" | RBX -> "%rbx" | RCX -> "%rcx" | RDX -> "%rdx"
    | RSI -> "%rsi" | RDI -> "%rdi" | RBP -> "%rbp" | RSP -> "%rsp"
    | R8  -> "%r8"  | R9  -> "%r9"  | R10 -> "%r10" | R11 -> "%r11"
    | R12 -> "%r12"

let asm_mem_to_string (mem : asm_mem) : string =
    match mem with
    | RBP_offset offset -> Printf.sprintf "%d(%%rbp)" offset
    | REG reg -> asm_reg_to_string reg
    | LABEL label -> "$" ^ label
    | IMMEDIATE i -> Printf.sprintf "$%d" i

let print_asm_cmd (output : string -> unit) (arg_count : int) (cmd : asm_cmd) : unit =
    let format_cmd1 (cmd : string) (arg1 : string) : unit =
        output @@ Printf.sprintf "\t%-10s%s" cmd arg1
    in

    let format_cmd2 (cmd : string) (arg1 : string) (arg2 : string) : unit =
        output @@ Printf.sprintf "\t%-10s%s, %s" cmd arg1 arg2
    in

    (match cmd with
    | FRAME (size) ->
        format_cmd1 "pushq" "%rbp";
        output "\n";
        format_cmd2 "movq" "%rsp" "%rbp";
        output "\n";

        (*
            Stack-alignment must be 16-byte aligned, the callee will allocate 8 bytes for the return
            address plus 8 bytes for each pushed argument. Here you allocate 8 bytes for saving
            %rbp, and so you need to make sure you are either subtracting 8 mod 16 or 0 mod 16 from
            %rsp to maintain the alignment. With an odd number of arguments, the frame will be aligned
            at procedure entry, so your %rsp adjustment needs to be 8 mod 16. With an even number of
            arguments, the frame will be misaligned, so your %rsp adjustment needs to be  mod 16.
         *)
        let adjusted_size = 
            if arg_count mod 2 = 1 then
                if size mod 16 = 8 then
                    size
                else
                    size + 8 
            else
                if size mod 16 = 0 then
                    size
                else
                    size + 8
        in                
        format_cmd2 "subq" (Printf.sprintf "$%d" adjusted_size) "%rsp"

    | MOV_reg (mem, reg) -> format_cmd2 "movq" (asm_mem_to_string mem) (asm_reg_to_string reg)
    | MOV_mem (reg, mem) -> format_cmd2 "movq" (asm_reg_to_string reg) (asm_mem_to_string mem)
    
    | ADD (mem, reg) -> format_cmd2 "addq" (asm_mem_to_string mem) (asm_reg_to_string reg)
    | SUB (mem, reg) -> format_cmd2 "subq" (asm_mem_to_string mem) (asm_reg_to_string reg)
    | MUL (mem, reg) -> format_cmd2 "imulq" (asm_mem_to_string mem) (asm_reg_to_string reg)
    | DIV reg -> format_cmd1 "idivq" (asm_reg_to_string reg)
    | XOR (reg1, reg2) -> format_cmd2 "xorq" (asm_reg_to_string reg1) (asm_reg_to_string reg2)
    | NEG reg -> format_cmd1 "negq" (asm_reg_to_string reg)

    | TEST (reg1, reg2) -> format_cmd2 "testq" (asm_reg_to_string reg1) (asm_reg_to_string reg2)
    | CMP (reg1, reg2) -> format_cmd2 "cmpq" (asm_reg_to_string reg1) (asm_reg_to_string reg2)
    | SETL   -> format_cmd1 "setl" "%al"
    | SETLE  -> format_cmd1 "setle" "%al"
    | SETE   -> format_cmd1 "sete" "%al"

    | PUSH reg       -> format_cmd1 "pushq" (asm_reg_to_string reg)
    | POP reg        -> format_cmd1 "popq" (asm_reg_to_string reg)

    | JMP label      -> format_cmd1 "jmp" label
    | JNZ label      -> format_cmd1 "jnz" label
    | JE label       -> format_cmd1 "je" label

    | CALL label     -> format_cmd1 "callq" label
    | RET            -> output "\tleave\n\tret"

    | MISC s         -> output @@ "\t" ^ s

    | LABEL label -> output @@ label ^ ":"
    
    | COMMENT s -> output @@ Printf.sprintf "\t# %s" s
    )
    
    ;

    output "\n"

let print_asm_method (_method : asm_method) (output : string -> unit) : unit =
    output @@ "\n";    
    output @@ Printf.sprintf "\t.section .rodata\n";

    List.iter (fun (id, s) ->
        output @@ Printf.sprintf "%s:\n" id;
        output @@ Printf.sprintf "\t.string \"%s\"\n" s;
        output @@ Printf.sprintf "\t.align 8\n";
    ) _method.string_literals;
    
    output @@ "\n";
    output @@ "\t.text\n";
    output @@ Printf.sprintf "\t.globl %s\n" _method.header;
    output @@ Printf.sprintf "\t.type %s, @function\n" _method.header;

    output @@ _method.header ^ ":\n";
    List.iter (print_asm_cmd output @@ _method.arg_count) _method.commands