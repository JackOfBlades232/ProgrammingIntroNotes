%include "../stud_io.inc"
global _start

section .text
_start: xor ecx, ecx    ; zero out bracket counter
        xor bl, bl      ; zero out middle imbalance flag
lp:     GETCHAR         ; read next char from input
        cmp eax, -1     ; check if eof
        jz quit         ; if eof, quit
        cmp eax, 10     ; check if eoln
        jz print        ; if so, go to print result of line
        cmp eax, '('    ; check if opening bracket
        jnz cmp2        ; if not, go to check if closing
        inc ecx         ; if it is, increment the counter
        jmp lp          ; and go to repeat loop
cmp2:   cmp eax, ')'    ; else, check if closing
        jnz lp          ; if also not, repeat loop
        dec ecx         ; if it is, decrement the counter
        cmp ecx, -1     ; check if the counter went below 0
        jnz lp          ; if it didnt, repeat loop
        or bl, 1        ; if it did, set flag to 1
        jmp lp          ; repeat loop
print:  cmp bl, 0       ; check if flag is 0
        jnz no          ; if it isnt, go to print NO
        cmp ecx, 0      ; check if counter is 0
        jnz no          ; if not, also print NO
        PRINT "YES"     ; if both are 0, print YES
        jmp nl          ; skip NO part
no:     PRINT "NO"      ; if not skipped, print NO
nl:     PUTCHAR 10      ; in both cases, newline after output
        jmp _start      ; repeat it all for next line
quit:   FINISH          ; exit with macro
