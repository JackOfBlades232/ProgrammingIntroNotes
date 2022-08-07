%include "../stud_io.inc"
global _start

section .text
_start: xor bl, bl      ; zero out in-word flag
lp:     GETCHAR         ; get next char from input 
        cmp al, -1      ; check if eof
        jz quit         ; if eof, go to quit
        cmp al, 33      ; check if lower than word chars
        jl wb           ; if so, go to deal with wordbreak
        cmp al, 126     ; check if higher than chars 
        jg wb           ; if so, go to deal with wordbreak
        cmp bl, 0       ; if a char, check if prev flag value was 0
        jnz pchar       ; if it wasnt, go to print the char
        PUTCHAR '('     ; if it was, first print a bracket
pchar:  PUTCHAR al      ; print the char
        mov bl, 1       ; set the in-word flag
        jmp lp          ; repeat
wb:     cmp bl, 1       ; if went to wordbreak, check prev flag val
        jz clbr         ; if it was set, go to print closing bracket
        PUTCHAR al      ; if it wasnt, just print the char
        jmp lp          ; and repeat
clbr:   PUTCHAR ')'     ; if it was, close the bracket 
        PUTCHAR al      ; and then print the char
        jmp _start      ; and repeat with flag reset 
quit:   FINISH          ; exit with macro 
