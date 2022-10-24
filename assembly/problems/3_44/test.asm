;; 3_44/test.asm ;;
%include "../useful_macros.inc"

global _start
extern write_float
extern putstr

section .data
number  dq 342.12
nl      db 10, 0
nl_ln   equ $-nl

section .bss
dgts    resb 19
dgts_ln equ $-dgts

section .text
_start: finit
        fld qword [number]
        mov eax, dgts
        mov ecx, dgts_ln
        call write_float
        cmp eax, 0
        jl .err
        kernel 4, 1, dgts, eax
        kernel 4, 1, nl, nl_ln
        kernel 1, 0
.err:   kernel 1, 1
