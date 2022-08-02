%include "../stud_io.inc"
global _start

section .text
_start: GETCHAR         ; read char from input
        cmp eax, -1     ; check if eof
        jz quit         ; if eof, jump out
        PUTCHAR al      ; output the char
        jmp _start      ; repeat
quit:   FINISH          ; exit with macro
