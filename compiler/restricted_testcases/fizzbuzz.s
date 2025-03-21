# -------- BUILT-IN FUNCTIONS ------------

# Start routine
    .globl main
    .type  main, @function
main:
    jmp   Main_main_0

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

	.section .rodata
str_0:
	.string ": "
	.align 8
str_1:
	.string "Fizz"
	.align 8
str_2:
	.string "Buzz"
	.align 8
str_3:
	.string "\n"
	.align 8

	.text
	.globl Main_main_0
	.type Main_main_0, @function
Main_main_0:
	pushq     %rbp
	movq      %rsp, %rbp
	subq      $328, %rsp
	movq      $0, %rax
	movq      %rax, -328(%rbp)
t$39_cond:
	movq      -328(%rbp), %rax
	movq      %rax, -296(%rbp)
	movq      $100, %rax
	movq      %rax, -288(%rbp)
	movq      -296(%rbp), %rbx
	movq      -288(%rbp), %rcx
	cmpq      %rcx, %rbx
	setl      %al
	movq      %rax, -304(%rbp)
	movq      -304(%rbp), %rax
	testq     %rax, %rax
	jnz       t$39_body
	jmp       t$39_merge
t$39_body:
	movq      -328(%rbp), %rax
	movq      %rax, -264(%rbp)
	movq      $1, %rax
	movq      %rax, -256(%rbp)
	movq      -264(%rbp), %rax
	movq      -256(%rbp), %rbx
	addq      %rax, %rbx
	movq      %rbx, -272(%rbp)
	movq      -272(%rbp), %rax
	movq      %rax, -328(%rbp)
	movq      -328(%rbp), %rax
	movq      %rax, -240(%rbp)
	movq      -240(%rbp), %rax
	pushq     %rax
	movq      %rax, -248(%rbp)
	callq     out_int
	popq      %rax
	movq      $str_0, %rax
	movq      %rax, -224(%rbp)
	movq      -224(%rbp), %rax
	pushq     %rax
	movq      %rax, -232(%rbp)
	callq     out_string
	popq      %rax
	movq      -328(%rbp), %rax
	movq      %rax, -184(%rbp)
	movq      $3, %rax
	movq      %rax, -176(%rbp)
	movq      -184(%rbp), %rax
	movq      -176(%rbp), %rbx
	CQO
	idivq     %rbx
	movq      %rax, -192(%rbp)
	movq      $3, %rax
	movq      %rax, -168(%rbp)
	movq      -192(%rbp), %rax
	movq      -168(%rbp), %rbx
	imulq     %rax, %rbx
	movq      %rbx, -200(%rbp)
	movq      -328(%rbp), %rax
	movq      %rax, -160(%rbp)
	movq      -200(%rbp), %rbx
	movq      -160(%rbp), %rcx
	xorq      %rax, %rax
	cmpq      %rbx, %rcx
	sete      %al
	movq      %rax, -208(%rbp)
	movq      -208(%rbp), %rax
	testq     %rax, %rax
	jnz       t$24_then
	jmp       t$24_else
t$24_then:
	movq      $str_1, %rax
	movq      %rax, -144(%rbp)
	movq      -144(%rbp), %rax
	pushq     %rax
	movq      %rax, -152(%rbp)
	callq     out_string
	popq      %rax
	movq      -152(%rbp), %rax
	movq      %rax, -216(%rbp)
	jmp       t$24_merge
t$24_else:
	movq      $0, %rax
	movq      %rax, -136(%rbp)
	movq      -136(%rbp), %rax
	movq      %rax, -216(%rbp)
t$24_merge:
	movq      -328(%rbp), %rax
	movq      %rax, -88(%rbp)
	movq      $5, %rax
	movq      %rax, -80(%rbp)
	movq      -88(%rbp), %rax
	movq      -80(%rbp), %rbx
	CQO
	idivq     %rbx
	movq      %rax, -96(%rbp)
	movq      $5, %rax
	movq      %rax, -72(%rbp)
	movq      -96(%rbp), %rax
	movq      -72(%rbp), %rbx
	imulq     %rax, %rbx
	movq      %rbx, -104(%rbp)
	movq      -328(%rbp), %rax
	movq      %rax, -64(%rbp)
	movq      -104(%rbp), %rbx
	movq      -64(%rbp), %rcx
	xorq      %rax, %rax
	cmpq      %rbx, %rcx
	sete      %al
	movq      %rax, -112(%rbp)
	movq      -112(%rbp), %rax
	testq     %rax, %rax
	jnz       t$36_then
	jmp       t$36_else
t$36_then:
	movq      $str_2, %rax
	movq      %rax, -48(%rbp)
	movq      -48(%rbp), %rax
	pushq     %rax
	movq      %rax, -56(%rbp)
	callq     out_string
	popq      %rax
	movq      -56(%rbp), %rax
	movq      %rax, -120(%rbp)
	jmp       t$36_merge
t$36_else:
	movq      $0, %rax
	movq      %rax, -40(%rbp)
	movq      -40(%rbp), %rax
	movq      %rax, -120(%rbp)
t$36_merge:
	movq      $str_3, %rax
	movq      %rax, -16(%rbp)
	movq      -16(%rbp), %rax
	pushq     %rax
	movq      %rax, -24(%rbp)
	callq     out_string
	popq      %rax
	jmp       t$39_cond
t$39_merge:
	movq      -312(%rbp), %rax
	leave
	ret
