# -------- INTRINSIC FUNCTIONS ------------

# Start routine
    .globl main
    .type  main, @function
main:
    callq   Main_main_0
    retq

# [rsp]         -> return address
# [rsp + 8]     -> string pointer
    .section .rodata
__print_val:
    .string "%s"

    .text
    .globl out_string
    .type  out_string, @function
out_string:
    movq    $__print_val, %rdi
    movq    8(%rsp), %rsi
    xorq    %rax, %rax
    jmp     printf

# -------- COMPILED PROGRAM START ------------

	.section .rodata
str_0:
	.string "Hello, world!\n"
	.align 8

	.text
	.globl Main_main_0
	.type Main_main_0, @function
Main_main_0:
	pushq     %rbp
	movq      %rsp, %rbp
	subq      $16, %rsp
	movq      $str_0, %rax
	movq      %rax, -8(%rbp)
	movq      -8(%rbp), %rax
	pushq     %rax
	callq     out_string
	movq      %rax, -16(%rbp)
	popq      %rax
	movq      -16(%rbp), %rax
	leave
	ret
