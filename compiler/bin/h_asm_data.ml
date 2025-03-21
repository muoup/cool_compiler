type asm_reg =
    | RAX   | RBX   | RCX   | RDX
    | RSI   | RDI   | RBP   | RSP
    | R8    | R9    | R10   | R11
    | R12

type asm_mem = 
    | RBP_offset    of int
    | REG           of asm_reg
    | LABEL         of string

type asm_cmd =
    | FRAME     of int 

    | MOV_reg   of asm_mem * asm_reg
    | MOV_mem   of asm_reg * asm_mem

    | ADD       of asm_mem * asm_reg
    | SUB       of asm_mem * asm_reg
    | MUL       of asm_mem * asm_reg
    | DIV       of asm_mem * asm_reg

    | PUSH      of asm_reg
    | POP       of asm_reg

    | CALL      of string
    | RET

    | LABEL     of string

    | COMMENT   of string

type asm_method = {
    header: string;

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

let print_asm_cmd (cmd : asm_cmd) (output : string -> unit) : unit =
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
        format_cmd2 "subq" (Printf.sprintf "$%d" size) "%rsp"

    | MOV_reg (mem, reg) -> format_cmd2 "movq" (asm_mem_to_string mem) (asm_reg_to_string reg)
    | MOV_mem (reg, mem) -> format_cmd2 "movq" (asm_reg_to_string reg) (asm_mem_to_string mem)
    
    | ADD (mem, reg) -> format_cmd2 "addq" (asm_mem_to_string mem) (asm_reg_to_string reg)
    | SUB (mem, reg) -> format_cmd2 "subq" (asm_mem_to_string mem) (asm_reg_to_string reg)
    | MUL (mem, reg) -> format_cmd2 "imulq" (asm_mem_to_string mem) (asm_reg_to_string reg)
    | DIV (mem, reg) -> format_cmd2 "idivq" (asm_mem_to_string mem) (asm_reg_to_string reg)

    | PUSH reg       -> format_cmd1 "pushq" (asm_reg_to_string reg)
    | POP reg        -> format_cmd1 "popq" (asm_reg_to_string reg)

    | CALL label     -> format_cmd1 "callq" label
    | RET            -> output "\tleave\n\tret"

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
    List.iter (fun cmd -> print_asm_cmd cmd output) _method.commands