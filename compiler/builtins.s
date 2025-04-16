# -------- BUILT-IN FUNCTIONS ------------
.section .note.GNU-stack,"",@progbits

#   Designed with help from GCC output, and some use of the reference compiler output

# Start routine
    .text
    .globl main
    .type  main, @function
main:
    .cfi_startproc
    push    %rbp
    movq    %rsp, %rbp
    .cfi_def_cfa_register 6
    .cfi_def_cfa_offset 16

    call    new.Main
    
    pushq   %rax        # Additional push for alignment
    pushq   %rax
    call    Main.main
    addq    $16, %rsp

    pop     %rbp
    ret
    .cfi_endproc

    .section .rodata
__f_out_str:
    .string "%s"
    .align 8

    .text
    .globl out_string
    .type  out_string, @function
#   1 arg (char*) -> 8 call bytes + 8 arg bytes = 16-bit aligned stack
out_string: 
    pushq   %rbp
    movq    %rsp, %rbp
    subq    $16, %rsp

    movq    16(%rbp), %rdi
    call    strlen
    
    leaq    2(%rax), %rdi        #   Add 2 so glibc doesn't yell at us
    call    malloc

    movq    %rax, -8(%rbp)       #   Store the pointer to the new string
    movq    %rax, %r8

    movq    16(%rbp), %r9

.out_loop:
    movzbl  (%r9), %eax
    incq    %r9

.out_char:
    testb   %al, %al
    je      .finish

    cmpb    $0x5C, %al       # If '\' then try to escape the character
    je      .escape_char

.put_char:
    movb    %al, (%r8)
    incq    %r8

    jmp     .out_loop

.escape_char:
    movzbl  (%r9), %eax
    incq    %r9

    cmpb    $0x6E, %al
    je      .escape_n

    cmpb    $0x74, %al
    je      .escape_t

    movb    $0x5C, (%r8)
    incq    %r8

    movzbl  -1(%r9), %eax    
    jmp     .out_char

.escape_n:
    movb    $0x0A, %al
    jmp     .put_char   

.escape_t:
    movb    $0x09, %al
    jmp     .put_char

.finish:
    movb    $0, (%r8)       #   Null terminate the string

    movq    $__f_out_str, %rdi
    movq    -8(%rbp), %rsi
    xorq    %rax, %rax

    call    printf

    leave
    ret

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
    movq    %r12, %rax
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
    movq    %rsp, %rbp
    subq    $16, %rsp

    movq    $0, 8(%rsp)
    movq    $0, (%rsp)

    leaq    8(%rsp), %rdi
    movq    %rsp, %rsi
    movq    stdin(%rip), %rdx
    callq   getline             # getline(char** buffer, int* n, FILE* stream)

    cmpq    $-1, %rax
    je      .in_string_fail

    movq    %rax, %rdi
    addq    8(%rsp), %rdi
    movq    $0, -1(%rdi)

    movq    8(%rsp), %rdi
    xorq    %rsi, %rsi
    leaq    -1(%rax), %rdx
    callq   memchr              # memchr(char* buffer, char find, int bytes)

    movq    $__empty_string, %rdi
    movq    %rax, %rsi
    movq    8(%rsp), %rax

    testq   %rsi, %rsi          # if memchr(...) == 0 then return $__empty_string
    cmovne  %rdi, %rax

    leave
    ret

.in_string_fail:
    movq    $__empty_string, %rax

    leave
    ret

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

    pop     %r12
    leave
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
    movq    24(%rbp), %rdi
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
    movq    24(%rbp), %rsi
    call    strcat

    #   Return the new string
    movq    8(%rsp), %rax
    addq    $16, %rsp
    pop     %r12
    leave
    ret

    .text
    .globl substr
    .type  substr, @function
substr:
    movq    %r12, %rdi
    call    strlen

    movq    24(%rbp), %rsi
    addq    32(%rbp), %rsi
    cmpq    %rax, %rsi
    jg      error_substr

    movq    32(%rbp), %rdi
    incq    %rdi
    call    malloc

    movq    32(%rbp), %rdi
    movq    $0, 1(%rdi, %rax)

    movq    %rax, %rdi
    movq    24(%rbp), %rsi
    leaq    (%r12, %rsi), %rsi
    movq    32(%rbp), %rdx
    call    memcpy

    pop     %r12
    leave
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
    movq    8(%rsp), %rdi
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
    movq    8(%rsp), %rdi
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
    movq    8(%rsp), %rdi
    movq    %rdi, 24(%rax)

    ret

    .text
    .globl  lift_int
    .type   lift_int, @function

lift_val:
    movq    8(%rsp), %rax
    movq    24(%rax), %rax
    ret

    .text
    .globl ambigious_compare
    .type  ambigious_compare, @function
ambigious_compare:
    push    %rbp
    movq    %rsp, %rbp
    subq    $16, %rsp

    movq    16(%rbp), %rax
    testq   %rax, %rax
    je      .standard_compare       # Case 1: Type1 == Void -> Standard Compare

    movq    (%rax), %rax
    movq    %rax, -8(%rbp)
    movq    %rax, %rdi
    
    movq    24(%rbp), %rax
    testq   %rax, %rax
    je      .standard_compare       # Case 2: Type2 == Void -> Standard Compare

    movq    (%rax), %rax
    movq    %rax, -16(%rbp)
    movq    %rax, %rsi

    callq   strcmp                  # Case 3: Type1 != Type2 -> Standard Pointer-based Compare
    testq   %rax, %rax
    jne     .standard_compare 

    movq    -8(%rbp), %rdi          # Case 4: Type1 = "Bool" -> Lift Value then Compare      
    movq    $.objname_Bool, %rsi
    callq   strcmp
    testq   %rax, %rax
    je      .int_bool_compare

    movq    -8(%rbp), %rdi          # Case 5: Type1 = "Int" -> Lift Value then Compare
    movq    $.objname_Int, %rsi
    callq   strcmp
    testq   %rax, %rax
    je      .int_bool_compare

    movq    -8(%rbp), %rdi          # Case 6: Type1 = "String" -> Lift Value then Compare
    movq    $.objname_String, %rsi
    callq   strcmp
    testq   %rax, %rax
    je      .string_compare

    jmp     .standard_compare       # Case 5: Type1 == Type2 and the Type is Unlifted -> Standard Compare

.string_compare:
    movq    16(%rbp), %rdi
    movq    24(%rdi), %rdi

    movq    24(%rbp), %rsi
    movq    24(%rsi), %rsi

    callq   strcmp

    leave
    ret

.int_bool_compare:
    movq    16(%rbp), %rdi
    movq    24(%rdi), %rdi
    movq    %rdi, 16(%rbp)

    movq    24(%rbp), %rsi
    movq    24(%rsi), %rsi
    movq    %rsi, 24(%rbp)

#   Fall through into the standard compare

.standard_compare:
    movq    16(%rbp), %rdi
    movq    24(%rbp), %rsi

    xorq    %rax, %rax

    cmpq    %rsi, %rdi
    movq    $1, %rdi
    movq    $-1, %rsi

    cmovg   %rdi, %rax
    cmovl   %rsi, %rax

    leave
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
error_case_msg:
    .string "ERROR: %d: Exception: case on void\n"
    .align 8
error_case_unmatched_msg:
    .string "ERROR: %d: Exception: no valid case for expression\n"
    .align 8
error_substring_msg:
    .string "ERROR: 0: Exception: String.substr out of range\n"
    .align 8

    .text
    .globl error_div_on_zero
    .type  error_div_on_zero, @function
error_divide_zero:
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

    movq    $1, %rdi
    callq   exit
    ret

    .text
    .globl error_case_void
    .type  error_case_void, @function
error_case_void:
    movq    $error_case_msg, %rdi
    xorq    %rax, %rax
    callq   printf

    movq    $1, %rdi
    callq   exit
    ret

    .text
    .globl error_case_unmatched
    .type  error_case_unmatched, @function
error_case_unmatched:
    movq    $error_case_unmatched_msg, %rdi
    xorq    %rax, %rax
    callq   printf

    movq    $1, %rdi
    callq   exit
    ret

    .text
    .globl error_substr
    .type  error_substr, @function
error_substr:
    movq    $error_substring_msg, %rdi
    xorq    %rax, %rax
    callq   printf

    movq    $1, %rdi
    callq   exit
    ret

# -------- COMPILED PROGRAM START ------------
