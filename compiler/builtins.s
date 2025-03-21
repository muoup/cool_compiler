# -------- BUILT-IN FUNCTIONS ------------

# Start routine
    .globl main
    .type  main, @function
main:
    callq   Main_main_0
    retq

    .section .rodata
__print_str:
    .string "%s"

    .text
    .globl out_string
    .type  out_string, @function
out_string:
    movq    $__print_str, %rdi
    movq    8(%rsp), %rsi
    xorq    %rax, %rax
    jmp     printf

    .section .rodata
__print_int:
    .string "%d"

    .text
    .globl out_int
    .type  out_int, @function
out_int:
    movq    $__print_int, %rdi
    movq    8(%rsp), %rsi
    xorq    %rax, %rax
    jmp     printf

# -------- COMPILED PROGRAM START ------------