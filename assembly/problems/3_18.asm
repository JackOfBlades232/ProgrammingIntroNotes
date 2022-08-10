; Here I presume that the number is not higher than 2^32

%include "../stud_io.inc"
global _start

section .text
_start: xor ecx, ecx    ; zero out register for the number
lp:     GETCHAR         ; read next (supposed) digit
        cmp al, '0'     ; check if char is below 0
        jl stars        ; if so, go to print stars
        cmp al, '9'     ; check if char is above 9
        jg stars        ; if so, go to print stars
        sub al, '0'     ; cast digit code in al to its value
        shl ecx, 1      ; multiply the number by 2
        lea ecx, [ecx*5]    ; and by 5, thus multiplying it by 10
        add ecx, eax    ; add the next digit
        jmp lp          ; repeat
stars:  cmp ecx, 0      ; check if the final number is zero
        jz quit         ; if so, just quit
lp1:    PUTCHAR '*'     ; if not, print the stars
        loop lp1        ; until counter/number is 0
        PUTCHAR 10      ; finalize output with new line
quit:   FINISH          ; and exit with macro
