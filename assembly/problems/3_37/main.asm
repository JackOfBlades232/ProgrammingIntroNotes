;; 3_37/main.asm ;;
%include "../useful_macros.inc"

global _start
extern read_oct
extern write_oct
extern putstr

section .data                      
nl_msg  db 10, 0
nl_len  equ $-nl_msg
sum_msg db "Sum (oct): ", 0
prd_msg db "Prod (oct): ", 0
discl   db "Cli args interpreted as oct numbers (only digits allowed)", 10, 0

section .bss
sum_dgt resb 12                     ; bytes for digits of sum&prod, 12th for #0
prd_dgt resb 12

section .text
; main code
_start: cmp byte [esp], 3           ; if not req number of args, quit 
        jnz .err
        pcall read_oct, dword [esp+8]
        test cl, cl                 ; read first num and check if term with #0
        jnz .err 
        mov ebx, eax                ; and store it in ebx
        pcall read_oct, dword [esp+12] 
        test cl, cl                 ; and second num
        jnz .err 
        mov edi, eax                ; store second in edi
        add eax, ebx                ; perform addition
        pcall write_oct, sum_dgt, eax   ; and write result digits
        mov eax, edi                ; and same for product (only 32 bytes)
        mul ebx 
        pcall write_oct, prd_dgt, eax   
        pcall putstr, discl         ; now, the outputs
        pcall putstr, sum_msg
        pcall putstr, sum_dgt
        kernel 4, 1, nl_msg, nl_len
        pcall putstr, prd_msg
        pcall putstr, prd_dgt
        kernel 4, 1, nl_msg, nl_len
        kernel 1, 0
.err:   kernel 1, 1 
