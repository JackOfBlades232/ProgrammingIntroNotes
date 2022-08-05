%include "../stud_io.inc"
global _start

section .text
_start: xor ecx, ecx    ; zero out word counter
clflg:  xor bl, bl      ; zero out inword flag
lp:     GETCHAR         ; read next char to eax
        cmp eax, -1     ; check if eof
        jz quit         ; if eof, quit
        cmp eax, 33     ; check if lower than word chars
        jl wb           ; if so, go to handle word break
        cmp eax, 126    ; check if higher than word chars
        jg wb           ; if so, go to handle word break
        mov bl, 1       ; if a char, set flag to 1
        jmp lp          ; repeat loop
wb:     cmp bl, 1       ; check if flag was 1 before word break
        jnz lpend       ; if it wasnt, go to loop end
        inc ecx         ; if it was, increment word counter
lpend:  cmp eax, 10     ; check if eoln
        jnz clflg       ; if not, go to reset flag
        cmp ecx, 0      ; else, check if word counter is zero
        jz _start       ; if it is 0, restart with next line
strlp:  PUTCHAR '*'     ; if it wasnt, print next *
        loop strlp      ; loop until counter is 0
        PUTCHAR 10      ; new line after output
        jmp _start      ; repeat for next line
quit:   FINISH          ; exit with macro
