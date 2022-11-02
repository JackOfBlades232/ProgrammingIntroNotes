;; 3_45/test.asm ;;
%include "../useful_macros.inc"

global _start
extern read_float
extern write_float

section .data
nl      db 10
nl_ln   equ $-nl

section .bss
dgts    resb 19
dgts_ln equ $-dgts

section .text
_start: cmp dword [esp], 3
        jnz .err
        finit

        mov eax, [esp+8]
        call read_float
        cmp eax, 0
        jnz .err
        mov eax, [esp+12]
        call read_float
        cmp eax, 0
        jnz .err

        faddp st1, st0

        mov eax, dgts        
        mov ecx, dgts_ln
        call write_float
        kernel 4, 1, dgts, eax
        kernel 4, 1, nl, nl_ln

        kernel 1, 0

.err:   kernel 1, 1
