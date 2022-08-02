%include "../stud_io.inc"
global _start

section .text
_start: GETCHAR         ; read from input to EAX
        cmp eax, 'A'    ; did we read A? 
        jz yes          ; if equal, jump to yes
        PRINT 'NO'      ; print NO
        jmp quit        ; skip rest of program
yes:    PRINT 'YES'     ; print YES
quit:   PUTCHAR 10      ; new line after output
        FINISH          ; exit with macro
