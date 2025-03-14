label Main_main_0
t$1 <- bool true
bt t$1 t$7_then
jmp t$7_else
label t$7_then
t$3 <- int 1
t$4 <- int 2
t$2 <- + t$3 t$4
t$5 <- t$2
jmp t$7_merge
label t$7_else
t$6 <- int 0
label t$7_merge
return t$0
