# -------- BUILT-IN FUNCTIONS ------------
.section .note.GNU-stack,"",@progbits

# Start routine
    .text
    .globl main
    .type  main, @function
main:
    push    %rbp

    call    new.Main
    
    pushq   %rax
    call     Main.main

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

    xorq    %rax, %rax
    movq    %rax, -8(%rbp)
    movq    $__f_int, %rdi
    leaq    -8(%rbp), %rsi
    callq   __isoc99_scanf

    movq    -8(%rbp), %rax
    addq    $16, %rsp
    popq    %rbp
    retq

    .section .rodata
__f_in_str:
    .string "%49s"
    .align 8
__test:
    .string "test"

    .text
    .globl in_string
    .type  in_string, @function
#   0 args      -> 8 call bytes = 8-byte off aligned stack
in_string:
    push    %rbp

    movq    $50, %rdi
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

    .text
    .globl type_name
    .type  type_name, @function
type_name:
    movq    8(%rbp), %rax
    movq    (%rax), %rax
    ret

# -------- COMPILED PROGRAM START ------------
