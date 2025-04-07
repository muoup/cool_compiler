# -------- BUILT-IN FUNCTIONS ------------
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
    addq    $8, %rsp

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
    .globl copy
    .type  copy, @function
copy:
    movq    8(%r12), %rdi
    call    malloc

    movq    %rax, %rdi
    movq    %r12, %rsi
    movq    8(%r12), %rdx
    call    memcpy

    leave
    pop     %r12
    ret

    .text
    .globl concat
    .type  concat, @function
concat:
    subq    $16, %rsp

    #   Store length of caller string into %rbx
    movq    %r12, %rdi
    call    strlen
    movq    %rax, %rbx

    #   Store length of string to be concatenated into %rax
    movq    32(%rbp), %rdi
    call    strlen
    
    #   Add the two lengths together to get the required space for the new string
    addq    %rbx, %rax
    incq    %rax            #   Add 1 for null terminator
    movq    %rax, %rdi
    call    malloc

    #   Null terminate the empty string
    movq    $0, (%rax)
    movq    %rax, 8(%rsp)

    #   Copy the first string into the new string
    movq    %rax, %rdi
    movq    %r12, %rsi
    call    strcat

    #   Copy the second string into the new string
    movq    8(%rsp), %rdi
    movq    32(%rbp), %rsi
    call    strcat

    #   Return the new string
    movq    8(%rsp), %rax
    leave
    pop     %r12
    ret

    .text
    .globl substr
    .type  substr, @function
substr:
    movq    %r12, %rdi
    call    strlen

    movq    32(%rbp), %rsi
    addq    40(%rbp), %rsi
    cmpq    %rax, %rsi
    jg      error_substr

    movq    40(%rbp), %rdi
    incq    %rdi
    call    malloc

    movq    40(%rbp), %rdi
    movq    $0, 1(%rdi, %rax)

    movq    %rax, %rdi
    movq    32(%rbp), %rsi
    leaq    (%r12, %rsi), %rsi
    movq    40(%rbp), %rdx
    call    memcpy

    leave
    pop     %r12
    ret

    .text
    .globl  unlift_int
    .type   unlift_int, @function
unlift_int:
    movq    $1, %rdi
    movq    $32, %rsi
    call    calloc

    movq    $.objname_Int, (%rax)
    movq    $32, 8(%rax)
    movq    $.vtable_Int, 16(%rax)
    movq    16(%rbp), %rdi
    movq    %rdi, 24(%rax)

    ret

    .text
    .globl  unlift_int
    .type   unlift_int, @function
unlift_string:
    movq    $1, %rdi
    movq    $32, %rsi
    call    calloc

    movq    $.objname_String, (%rax)
    movq    $32, 8(%rax)
    movq    $.vtable_String, 16(%rax)
    movq    16(%rbp), %rdi
    movq    %rdi, 24(%rax)

    ret

    .text
    .globl  unlift_int
    .type   unlift_int, @function
unlift_bool:
    movq    $1, %rdi
    movq    $32, %rsi
    call    calloc

    movq    $.objname_Bool, (%rax)
    movq    $32, 8(%rax)
    movq    $.vtable_Bool, 16(%rax)
    movq    16(%rbp), %rdi
    movq    %rdi, 24(%rax)

    ret

    .text
    .globl  lift_int
    .type   lift_int, @function
lift_int:
    movq    16(%rbp), %rax
    movq    24(%rax), %rax
    ret

    .section .rodata
default_string:
    .string ""
    .align 8
abort_msg:
    .string "abort";
error_divide_msg:
    .string "ERROR: %d: Exception: division by zero\n"
    .align 8
error_dispatch_msg:
    .string "ERROR: %d: Exception: dispatch on void\n"
    .align 8
error_substring_msg:
    .string "ERROR: 0: Exception: String.substr out of range\n"
    .align 8

    .text
    .globl error_div_on_zero
    .type  error_div_on_zero, @function
error_div_zero:
    movq    $error_divide_msg, %rdi
    xorq    %rax, %rax
    callq   printf

    movq    $1, %rdi
    callq   exit
    ret

    .text
    .globl error_dispatch
    .type  error_dispatch, @function
error_dispatch:
    movq    $error_dispatch_msg, %rdi
    xorq    %rax, %rax
    callq   printf

    int3

    movq    $1, %rdi
    callq   exit
    ret
error_substr:
    movq    $error_substring_msg, %rdi
    xorq    %rax, %rax
    callq   printf

    movq    $1, %rdi
    callq   exit
    ret

# -------- COMPILED PROGRAM START ------------
