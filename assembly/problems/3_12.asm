%include "../stud_io.inc"
global _start

section .text
_start: GETCHAR         ; read char from input
        cmp eax, -1     ; check if eof
        jz quit         ; if eof, jump out
        cmp eax, 10     ; check of eoln
        jnz _start      ; if not, repeat
        PRINT "OK"      ; print ok after eoln
        PUTCHAR 10      ; new line
        jmp _start      ; repeat
quit:   FINISH          ; exit with macro
