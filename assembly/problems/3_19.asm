; Here I presume that the length of the text is less than 2^32

%include "../stud_io.inc"
global _start

section .bss
digits  resb 10

section .text
_start: xor ebx, ebx    ; zero out accumulator
lp1:    GETCHAR         ; read next char of text
        cmp eax, -1     ; check if eof
        jz dgs          ; if so, go to calc digits of text length
        cmp eax, 10     ; if not eof, check if eoln
        jz lp1          ; if so, continue reading text
        inc ebx         ; else, increment text length accumulator
        jmp lp1         ; and repeat the loop
dgs:    mov eax, ebx    ; mov the length to newly freed eax
        xor edx, edx    ; zero out edx for correct division
        mov ebx, 10     ; put the divisor (for digits) in ebx
        mov ecx, 10     ; put the max number of digits in counter
lp2:    div ebx         ; perform int division
        mov [digits+ecx-1], dl  ; put the remainder (digit) in array (inv dir)
        xor dl, dl      ; clear remainder (no more than 10, so just dl)
        loop lp2        ; repeat the loop
        mov esi, digits ; put the array start adress in esi
        cld             ; set normal direction
        mov ecx, 10     ; again, put the max number of digits in cycle counter
        xor bl, bl      ; and clear a custom flag for the first non-zero digit
lp3:    lodsb           ; read digit to al and increment esi
        cmp bl, 0       ; check if there were non-zero digits before
        jnz prdgt       ; if so, go to print the digit
        cmp al, 0       ; if not, check if this one is non-zero
        jnz prdgt       ; if it is, go to print it
        loop lp3        ; else, repeat loop
prdgt:  or bl, 1        ; set the non-zero flag
        add al, '0'     ; convert digit to char
        PUTCHAR al      ; and output it
        loop lp3        ; repeat loop
        PUTCHAR 10      ; newline after output
quit:   FINISH          ; quit via macro
