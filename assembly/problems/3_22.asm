%include "../stud_io.inc"
global _start

section .text
_start: xor ecx, ecx
lp1:    GETCHAR             ; read next char from input
        cmp eax, -1         ; check if eof
        jz print            ; if eof, go to print
        cmp eax, 10         ; check if eoln
        jz print            ; if so, go to print
        inc ecx             ; increment char counter
        push eax            ; put char to stack
        jmp lp1             ; repeat input loop
print:  jecxz quit          ; if counter=0, output nothing
lp2:    pop eax             ; pop next char to eax (reverse order)
        PUTCHAR al          ; output it
        loop lp2            ; loop until ecx is 0
        PUTCHAR 10          ; if output not empty, new line
quit:   FINISH              ; exit with macro
