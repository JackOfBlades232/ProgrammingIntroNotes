;; 3_44/test.asm ;;
%include "../useful_macros.inc"

global _start
extern write_float

section .data
numbers dq 3.14, 1123.992, -0.0001, 99.999, 12.12234, -444.444444444444444, 0.0
num_ln  equ ($-numbers)/8
nl      db 10, 0
nl_ln   equ $-nl

section .bss
dgts    resb 19
dgts_ln equ $-dgts

section .text
_start: mov esi, numbers
        mov edi, num_ln
.again: finit
        fld qword [esi]
        mov eax, dgts
        mov ecx, dgts_ln
        call write_float
        kernel 4, 1, dgts, eax
        kernel 4, 1, nl, nl_ln
        add esi, 8
        dec edi
        cmp edi, 0
        jg .again
        kernel 1, 0
.err:   kernel 1, 1
