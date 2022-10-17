;; 3_38/main.asm ;;
%include "../useful_macros.inc"
%include "conversion_macro.inc"

global _start
extern read_num
extern write_num
extern putstr

section .data
rq_argc equ 4                       ; req 4 args (name, and 3 other args)
nl_msg  db 10, 0
nl_len  equ $-nl_msg

section .bss
base_1  resd 1
base_2  resd 1
number  resd 1
digits  resb 33

section .text
; main code : take 3 cli args, first 2 are digit system bases from 2 to 35 (Z)
;   the third is a number in the first system. Output the digit in the second
;   system if input is correct, otherwise terminate with code 1
_start: cmp dword [esp], rq_argc    ; check if argc as required
        jnz .err
        mov esi, esp                ; esi will point to next arg
        mov eax, [esi+8]            ; store both bases
        mov bl, [eax]
        char2num bl, .store_1, .err
.store_1:
        mov byte [base_1], bl
        inc eax 
        cmp byte [eax], 0           ; if base arg is longer than 1 char, error
        jnz .err 
        mov eax, [esi+12]              
        mov bl, [eax]
        char2num bl, .store_2, .err
.store_2:
        mov byte [base_2], bl
        inc eax 
        cmp byte [eax], 0           ; if base arg is longer than 1 char, error
        jnz .err 
        pcall read_num, [esi+16], [base_1]
        test cl, cl                 ; check if out code is 0
        jnz .err
        mov [number], eax
        pcall write_num, [number], digits, [base_2]
        test cl, cl                 
        jnz .err
        pcall putstr, digits
        kernel 4, 1, nl_msg, nl_len
        jmp .quit
.err:   kernel 1, 1
.quit:  kernel 1, 0
