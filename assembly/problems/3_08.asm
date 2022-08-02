%include "../stud_io.inc"
global _start

section .text
_start  GETCHAR         ; read char from input
        cmp eax, '9'    ; is it higher than 9?
        jg quit         ; if yes, quit
        cmp eax, '0'    ; is it lower than 0?
        jl quit         ; if yes, quit
        sub eax, '0'    ; substitute char with number
        mov ecx, eax    ; set loop counter
lp:     PUTCHAR '*'     ; print star
        loop lp         ; for iterations
        PUTCHAR 10      ; new line after output
quit:   FINISH          ; exit with macro
