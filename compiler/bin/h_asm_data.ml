type asm_reg =
    | RAX   | RBX   | RCX   | RDX
    | RSI   | RDI   | RBP   | RSP
    | R8    | R9    | R10   | R11
    | R12

type asm_mem = 
    | REG_offset    of asm_reg * int
    | REG           of asm_reg
    | LABEL         of string
    | IMMEDIATE     of int

type jmp_type =
    | JE
    | JNE
    | JG
    | JGE
    | JL
    | JLE

type asm_cmd =
    | FRAME     of { stack_size : int; regs : asm_reg list }

    | MOV       of asm_mem * asm_mem
    | MOV32     of asm_mem * asm_mem

    | ADD3      of asm_mem * asm_mem * asm_mem

    | ADD       of asm_mem * asm_reg
    | SUB       of asm_mem * asm_reg
    | MUL       of asm_mem * asm_reg
    | DIV       of asm_reg
    | XOR       of asm_reg * asm_reg

    | NEG       of asm_mem
    | NOT       of asm_reg

    | TEST      of asm_mem * asm_mem
    | CMP       of asm_mem * asm_mem

    | PUSH      of asm_mem
    | POP       of asm_reg

    | CALL      of string
    | CALL_indirect of asm_reg
    | JMP       of string
    | JNZ       of string
    | JZ        of string
    | JE        of string

    | JMPCC     of jmp_type * string
    | SETCC     of jmp_type

    | RET       of { regs : asm_reg list }

    | MISC      of string

    | LABEL     of string

    | COMMENT   of string

type asm_method = {
    class_name  : string;
    header      : string;
    arg_count   : int;

    commands    : asm_cmd list;
    string_literals: (string * string) list;
}

let asm_reg_to_string (reg : asm_reg) : string =
    match reg with
    | RAX -> "%rax" | RBX -> "%rbx" | RCX -> "%rcx" | RDX -> "%rdx"
    | RSI -> "%rsi" | RDI -> "%rdi" | RBP -> "%rbp" | RSP -> "%rsp"
    | R8  -> "%r8"  | R9  -> "%r9"  | R10 -> "%r10" | R11 -> "%r11"
    | R12 -> "%r12"

let asm_reg32_to_string (reg : asm_reg) : string =
    match reg with
    | RAX -> "%eax" | RBX -> "%ebx" | RCX -> "%ecx" | RDX -> "%edx"
    | RSI -> "%esi" | RDI -> "%edi" | RBP -> "%ebp" | RSP -> "%esp"
    | R8  -> "%r8d"  | R9  -> "%r9d"  | R10 -> "%r10d" | R11 -> "%r11d"
    | R12 -> "%r12d"

let asm_mem_to_string (mem : asm_mem) : string =
    match mem with
    | REG_offset (reg, offset) -> Printf.sprintf "%d(%s)" offset @@ asm_reg_to_string reg
    | REG reg -> asm_reg_to_string reg
    | LABEL label -> "$" ^ label
    | IMMEDIATE i -> Printf.sprintf "$%d" i

let asm_mem32_to_string (mem : asm_mem) : string =
    match mem with
    | REG reg -> asm_reg32_to_string reg
    | REG_offset (reg, offset) -> Printf.sprintf "%d(%s)" offset @@ asm_reg_to_string reg
    | LABEL label -> "$" ^ label
    | IMMEDIATE i -> Printf.sprintf "$%d" i

let print_asm_cmd (output : string -> unit) (arg_count : int) (cmd : asm_cmd) : unit =
    let format_cmd1 (cmd : string) (arg1 : string) : unit =
        output @@ Printf.sprintf "\t%-10s%s" cmd arg1
    in

    let format_cmd2 (cmd : string) (arg1 : string) (arg2 : string) : unit =
        output @@ Printf.sprintf "\t%-10s%s, %s" cmd arg1 arg2
    in

    let format_custom (cmd : string) (args : string) : unit =
        output @@ Printf.sprintf "\t%-10s%s" cmd args
    in

    (match cmd with
    | FRAME { stack_size; regs } ->
        output "# -- FRAME INITIALIZATION --\n";
        output @@ "# -- Stack Size Requested: " ^ string_of_int stack_size ^ "\n";
        format_cmd1 "pushq" "%rbp";
        output "\n";
        format_cmd2 "movq" "%rsp" "%rbp";
        output "\n";
        output "\t.cfi_def_cfa_register 16\n";
        output "\t.cfi_offset 6, 16\n";

        let total_alloc = 8 (* pushq %r12 *) + stack_size in
        let stack_size = stack_size - 8 * (List.length regs) in

        (* Updated: Call procedure now ensures 16-bit alignment in stack-memory for parameters passed *)
        let adjusted_size =
            if total_alloc mod 16 = 0 then
                stack_size
            else
                stack_size + 8
        in
        
        if adjusted_size > 0 then
            format_cmd2 "subq" (Printf.sprintf "$%d" adjusted_size) "%rsp\n"
        ;

        List.iter (fun reg ->
            format_cmd1 "pushq" (asm_reg_to_string reg);
            output "\n";
        ) regs;
        
        format_cmd1 "pushq" "%r12";
        output "\n";
        format_cmd2 "movq" "16(%rbp)" "%r12";
        output "\n";
        output "# -- FUNCTION START --"
    | MOV     (mem1, mem2) ->
        begin match mem1, mem2 with
        | REG reg1, _ ->
            format_cmd2 "movq" (asm_reg_to_string reg1) (asm_mem_to_string mem2)
        | _, REG reg2 ->
            format_cmd2 "movq" (asm_mem_to_string mem1) (asm_reg_to_string reg2)
        | LABEL _, _
        | IMMEDIATE _, _ ->
            format_cmd2 "movq" (asm_mem_to_string mem1) (asm_mem_to_string mem2)
        | _, _ ->
            format_cmd2 "movq" (asm_mem_to_string mem1) (asm_reg_to_string RAX);
            output "\n";
            format_cmd2 "movq" (asm_reg_to_string RAX) (asm_mem_to_string mem2) 
        end

    | MOV32     (mem1, mem2) ->
        begin match mem1, mem2 with
        | REG reg1, _ ->
            format_cmd2 "movl" (asm_reg32_to_string reg1) (asm_mem32_to_string mem2)
        | _, REG reg2 ->
            format_cmd2 "movl" (asm_mem32_to_string mem1) (asm_reg32_to_string reg2)
        | LABEL _, _
        | IMMEDIATE _, _ ->
            format_cmd2 "movl" (asm_mem32_to_string mem1) (asm_mem32_to_string mem2)
        | _, _ ->
            format_cmd2 "movl" (asm_mem32_to_string mem1) (asm_reg32_to_string RAX);
            output "\n";
            format_cmd2 "movl" (asm_reg32_to_string RAX) (asm_mem32_to_string mem2) 
        end

    | ADD (mem, reg) -> format_cmd2 "addq" (asm_mem_to_string mem) (asm_reg_to_string reg)

    | ADD3 (op1, op2, dest) ->
        begin match op1, op2, dest with
        | IMMEDIATE i, REG r1, REG r2
        | REG r1, IMMEDIATE i, REG r2 ->
            format_custom "leaq" (Printf.sprintf "%d(%s), %s" i (asm_reg_to_string r1) (asm_reg_to_string r2));
        | REG r1, REG r2, REG r3 ->
            format_custom "leaq" (Printf.sprintf "(%s, %s), %s" (asm_reg_to_string r1) (asm_reg_to_string r2) (asm_reg_to_string r3));
        | _ ->
            format_cmd2 "movq" (asm_mem_to_string op1) (asm_reg_to_string RAX);
            output "\n";
            format_cmd2 "addq" (asm_mem_to_string op2) (asm_reg_to_string RAX);
            output "\n";
            format_cmd2 "movq" (asm_reg_to_string RAX) (asm_mem_to_string dest)
        end

    | SUB (mem, reg) -> format_cmd2 "subq" (asm_mem_to_string mem) (asm_reg_to_string reg)
    | MUL (mem, reg) -> format_cmd2 "imulq" (asm_mem_to_string mem) (asm_reg_to_string reg)
    | DIV reg -> format_cmd1 "idivl" (asm_reg32_to_string reg)
    | XOR (reg1, reg2) -> format_cmd2 "xorq" (asm_reg_to_string reg1) (asm_reg_to_string reg2)
    | NEG mem -> format_cmd1 "negq" (asm_mem_to_string mem)

    | TEST (mem1, mem2) -> 
        begin match mem1, mem2 with
        | _, REG _
        | LABEL _, _ ->
            format_cmd2 "testl" (asm_mem32_to_string mem1) (asm_mem32_to_string mem2)
        | REG _, _
        | _, LABEL _ ->
            format_cmd2 "testl" (asm_mem32_to_string mem2) (asm_mem32_to_string mem1)
        | l, r when l = r ->
            format_cmd2 "movl"  (asm_mem32_to_string l) (asm_reg32_to_string RAX);
            output "\n";
            format_cmd2 "testl" (asm_reg32_to_string RAX) (asm_reg32_to_string RAX);
        | _ ->
            format_cmd2 "movl"  (asm_mem32_to_string mem2) (asm_reg32_to_string RAX);
            output "\n";
            format_cmd2 "testl" (asm_mem32_to_string mem1) (asm_reg32_to_string RAX)
        end 
    | CMP (mem1, mem2) -> 
        begin match mem1, mem2 with
        | _, REG r2 ->
            format_cmd2 "cmpl" (asm_mem32_to_string mem1) (asm_reg32_to_string r2)
        | _ ->
            format_cmd2 "movl" (asm_mem32_to_string mem2) (asm_reg32_to_string RAX);
            output "\n";
            format_cmd2 "cmpl" (asm_mem32_to_string mem1) (asm_reg32_to_string RAX)
        end

    | PUSH reg       -> format_cmd1 "pushq" (asm_mem_to_string reg)
    | POP reg        -> format_cmd1 "popq" (asm_reg_to_string reg)

    | JMP label      -> format_cmd1 "jmp" label
    | JNZ label      -> format_cmd1 "jnz" label
    | JZ label       -> format_cmd1 "jz" label
    | JE label       -> format_cmd1 "je" label

    | JMPCC (_type, label) ->
        begin match _type with
        | JE        -> format_cmd1 "je" label
        | JNE       -> format_cmd1 "jne" label
        | JG        -> format_cmd1 "jg" label
        | JGE       -> format_cmd1 "jge" label
        | JL        -> format_cmd1 "jl" label
        | JLE       -> format_cmd1 "jle" label
        end
    | SETCC _type ->
        begin match _type with
        | JE        -> format_cmd1 "sete"   "%al"
        | JNE       -> format_cmd1 "setne"  "%al"
        | JG        -> format_cmd1 "setg"   "%al"
        | JGE       -> format_cmd1 "setge"  "%al"
        | JL        -> format_cmd1 "setl"   "%al"
        | JLE       -> format_cmd1 "setle"  "%al"
        end

    | CALL label     -> format_cmd1 "callq" label
    | CALL_indirect reg -> format_cmd1 "callq" ("*" ^ asm_reg_to_string reg)
    | RET   { regs } ->
        format_cmd1 "pop" "%r12";
        output "\n";

        regs
        |> List.rev
        |> List.iter (fun reg ->
            format_cmd1 "popq" (asm_reg_to_string reg);
            output "\n";
        );

        output "\n\tleave\n\tret";

    | MISC s         -> output @@ "\t" ^ s

    | LABEL label -> output @@ label ^ ":"
    
    | COMMENT s -> output @@ Printf.sprintf "\t# %s" s

    | NOT n -> output @@ Printf.sprintf "not" (* so it doesn't have yellow lines on the enture file *)
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
    output @@ "\t.cfi_startproc\n";
    List.iter (print_asm_cmd output @@ _method.arg_count) _method.commands;
    output @@ "\t.cfi_endproc\n";