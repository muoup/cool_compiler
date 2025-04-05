let builtin_asm = {|# -------- BUILT-IN FUNCTIONS ------------
.section .note.GNU-stack,"",@progbits

#   Designed with help from GCC output, and some use of the reference compiler output

# Start routine
    .text
    .globl main
    .type  main, @function
main:
    push    %rbp

    call    new.Main
    
    pushq   %rax
    call    Main.main
    pop     %rax

    pop     %rbp
    ret

    .section .rodata
__f_out_str:
    .string "%s"
    .align 8

    .text
    .globl out_string
    .type  out_string, @function
#   1 arg (char*) -> 8 call bytes + 8 arg bytes = 16-bit aligned stack
out_string:
    movq    $__f_out_str, %rdi
    movq    8(%rsp), %rsi
    xorq    %rax, %rax
    callq   printf
    retq

    .section .rodata
__f_out_int:
    .string "%d"
    .align 8

    .text
    .globl out_int
    .type  out_int, @function
#   1 arg (int) -> 8 call bytes + 8 arg bytes = 16-bit aligned stack
out_int:
    movl    8(%rsp), %eax
    cdqe

    movq    $__f_out_int, %rdi
    movq    %rax, %rsi
    xorq    %rax, %rax
    
    call    printf
    ret

    .section .rodata
__f_in_int:
    .string " %ld"
    .align 8

    .text
    .globl in_int
    .type  in_int, @function
#   0 args      -> 8 call bytes = 8-byte off aligned stack
in_int:
    pushq   %rbp
    movq    %rsp, %rbp
    subq    $16, %rsp
    
    movq    $4096, %rdi
    movq    $1, %rsi
    callq   calloc

    movq    %rax, -8(%rbp)

    movq    %rax, %rdi
    movq    $4096, %rsi
    movq    stdin(%rip), %rdx
    callq   fgets

    movq    -8(%rbp), %rdi 
    movq    $__f_in_int, %rsi
    leaq    -16(%rbp), %rdx
    xorq    %rax, %rax
    callq   sscanf

    cmpl    $1, %eax            # if scanf fails, then fail
    jne     .in_int_fail

    movq    -16(%rbp), %rax

    cmpq    $2147483647, %rax   # if the integer is too large, then fail
    jg      .in_int_fail

    cmpq    $-2147483648, %rax  # if the integer is too small, then fail
    jl      .in_int_fail

    leave
    retq

.in_int_fail:
    movq    $0, %rax
    
    popq    %r8
    leave
    retq

    .section .rodata
__f_in_str:
    .string "%42959s"
    .align 8
__empty_string:
    .string ""

    .text
    .globl in_string
    .type  in_string, @function
#   0 args      -> 8 call bytes = 8-byte off aligned stack
in_string:
    pushq   %rbp
    pushq   %r8
    pushq   %r9

    movq    $42960, %rdi
    xorq    %rax, %rax
    callq   malloc

    movq    $0, %rbp            #   Flag representing if the string is invalid
    movq    %rax, %r8           #   Store string pointer in %r8
    movq    %rax, %r9           #   Store copy for iteration in %r9

.loop_begin:
    xorq    %rax, %rax
    movq    stdin(%rip), %rdi
    callq   fgetc

    testl   %eax, %eax      #   If a \0 is detected, set the invalid flag
    je      .fail_flag   

    cmpl    $0xA, %eax      #   If a \n is detected, check whether to stop reading
    je      .in_complete

    cmpl    $-1, %eax       #   If EOF is detected, stop reading
    je      .in_complete

    movb    %al, (%r9)      #   Otherwise, store the character in the string and increment the pointer
    incq    %r9
    jmp     .loop_begin

.fail_flag:
    movq    $1, %rbp        #   Set the invalid flag, however consume the rest of the characters
    jmp     .loop_begin     #   to flush the input buffer

.try_end:
    testq   %r9, %r8        #   If a new line is found at the beginning of the string, skip
    je      .loop_begin     #   Otherwise, complete

.in_complete:
    testq   %rbp, %rbp
    jnz     .in_fail        #   If the string is invalid, return empty string

    movb    $0, (%r9)       #   Add null terminator
    movq    %r8, %rax

    popq    %r9
    popq    %r8
    popq    %rbp
    retq

.in_fail:
    movq    $__empty_string, %rax

    popq    %r9
    popq    %r8
    popq    %rbp
    retq

    .text
    .globl type_name
    .type  type_name, @function
type_name:
    movq    24(%rbp), %rax
    movq    (%rax), %rax
    ret

# -------- COMPILED PROGRAM START ------------|}
