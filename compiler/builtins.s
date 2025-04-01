# -------- BUILT-IN FUNCTIONS ------------
.section .note.GNU-stack,"",@progbits

# Start routine
    .text
    .globl main
    .type  main, @function
main:
    jmp   Main_main_0

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
__f_int:
    .string "%d"

    .text
    .globl out_int
    .type  out_int, @function
#   1 arg (int) -> 8 call bytes + 8 arg bytes = 16-bit aligned stack
out_int:
    movq    $__f_int, %rdi
    movq    8(%rsp), %rsi
    xorq    %rax, %rax
    callq   printf
    retq

    .text
    .globl in_int
    .type  in_int, @function
#   0 args      -> 8 call bytes = 8-byte off aligned stack
in_int:
    pushq   %rbp
    movq    %rsp, %rbp
    subq    $16, %rsp

    callq   in_string
    movq    %rax, -8(%rbp)  # store the pointer to the string in -8(%rbp)
    movq    %rax, %rdi

    xorq    %rsi, %rsi
    movq    $10, %rdx    
    callq   strtol

    movq    %rax, %rbp

    callq   __errno_location
    cmpq    $34, %rax
    je     .in_int_fail

    movq    %rbp, %rax

    cmpq    $2147483647, %rax
    jg      .in_int_fail

    cmpq    $-2147483648, %rax
    jl      .in_int_fail

    addq    $16, %rsp
    popq    %rbp
    retq

.in_int_fail:
    movq    $0, %rax
    addq    $16, %rsp
    popq    %rbp
    retq

    .section .rodata
__f_in_str:
    .string "%42959s"
    .align 8
__test:
    .string "test"

    .text
    .globl in_string
    .type  in_string, @function
#   0 args      -> 8 call bytes = 8-byte off aligned stack
in_string:
    push    %rbp

    movq    $42960, %rdi
    xorq    %rax, %rax
    callq   malloc

    movq    $__f_in_str, %rdi
    movq    %rax, %rsi
    movq    %rax, %rbx
    xorq    %rax, %rax
    callq   __isoc99_scanf

    movq    %rbx, %rax
    popq    %rbp
    retq

# -------- COMPILED PROGRAM START ------------