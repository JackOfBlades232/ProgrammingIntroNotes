;; 3_43.asm ;;
%include "useful_macros.inc"

global _start

section .data
divisor equ 3
ok_msg  db "OK", 10, 0
ok_ln   equ $-ok_msg
argc_er db "Please pass one decimal number as cli arg!", 10, 0
argc_ln equ $-argc_er

section .text
_start: cmp dword [esp], 2      ; check if only name and number were passed
        jnz .argc_err
        mov esi, [esp+8]        ; esi to read the number
        xor al, al              ; accumulation of digit sum in al
        mov bl, divisor         ; and divisor in bl
.again: mov cl, [esi]           ; next char to cl
        cmp cl, 0
        jz .fin
        cmp cl, '0'
        jb .argc_err
        cmp cl, '9'
        ja .argc_err
        sub cl, '0'
        add al, cl
        xor ah, ah              ; prep ax for division
        div bl
        mov al, ah
        inc esi
        jmp .again
.fin:   cmp al, 0
        jnz .quit
        kernel 4, 1, ok_msg, ok_ln
.quit:  kernel 1, 0
.argc_err:
        kernel 4, 1, argc_er, argc_ln
        kernel 1, 1
