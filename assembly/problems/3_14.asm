%include "../stud_io.inc"
global _start

section .text
_start: xor bx, bx      ; clear space for word lens
        xor dl, dl      ; clear accumulator
lp1:    GETCHAR         ; read next char from input
        cmp eax, -1     ; check if eof
        jz clear        ; if eof, go to clear accumulator  
        cmp eax, 10     ; check if eoln
        jz clear        ; if eoln, go to clear accumulator
        cmp eax, 9      ; check if tab
        jz clear        ; if tab, go to clear accumulator
        cmp eax, ' '    ; check if space
        jz clear        ; if space, go to clear accumulator
        inc dl          ; increment accumulator if normal char
        jmp lp1         ; repeat loop with read char
clear:  cmp dl, 0       ; check if accumulator is empty
        jnz clear2      ; if not empty, go to second stage of clear
        cmp eax, -1     ; check if eof
        jz stars        ; if eof, go to print stars
        cmp eax, 10     ; check if eoln
        jz stars        ; again, go to print stars
        jmp lp1         ; repeat char loop
clear2: mov bl, dl      ; move accumulator to last word len place
        xor dl, dl      ; reset accumulator
        cmp bl, bh      ; compare last word to longest
        jle lpend       ; if last word not longer, go to loop end
        mov bh, bl      ; otherwise, put new longest word len in bh
lpend:  cmp eax, -1     ; again, check eof
        jz stars        ; if eof, print stars
        cmp eax, 10     ; check eoln
        jz stars        ; if eoln, print stars
        jmp lp1         ; if none of them, repeat char loop
stars:  xor ecx, ecx    ; clear counter
        cmp bh, 0       ; check if longest word is 0 len
        jz fin          ; if so, go to string read finale
        mov cl, bh      ; else, put len in counter
lp3:    PUTCHAR '*'     ; output next star
        loop lp3        ; loop until counter is 0
        PUTCHAR 10      ; new line after stars
stars2: mov cl, bl      ; put last word len in counter
lp4:    PUTCHAR '*'     ; output next star
        loop lp4        ; loop until counter is 0
        PUTCHAR 10      ; new line after stars
fin:    cmp eax, 10     ; check if eoln (not eof)
        jz _start       ; if so, repeat it all
        FINISH          ; else, exit with macro
