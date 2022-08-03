%include "../stud_io.inc"
global _start

section .text
_start: xor ecx, ecx    ; reset counter
cycle:  GETCHAR         ; read char from input
        cmp eax, -1     ; check if eof
        jz quit         ; if eof, jump out
        inc ecx         ; increment counter
        cmp eax, 10     ; check of eoln
        jnz cycle       ; if not, repeat
        cmp ecx, 1      ; check if there is only #10
        jz _start       ; if so, go to new line
        dec ecx         ; count out #10
lp:     PUTCHAR '*'     ; print next star
        loop lp         ; while counter is >0
        PUTCHAR 10      ; new line
        jmp _start      ; repeat all
quit:   FINISH          ; exit with macro
