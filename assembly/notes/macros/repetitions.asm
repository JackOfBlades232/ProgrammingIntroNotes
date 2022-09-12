%include "../../stud_io.inc"    ; macrodirective for including text
global _start

section .data
; macrocycle for generationg code, reserves 100 bytes with numbers 5-149
%assign n 50
%rep 100
        db n
    %assign n n+1
%endrep
; same, but for fib numbers and with assigning label
fibonacci
%assign i 1
%assign j 1
%rep 100000
    %if j > 100000
        %exitrep
    %endif

        dd j

    %assign k j+1
    %assign i j
    %assign j k
%endrep
fib_cnt equ ($-fibonacci)/4

section .bss
array   resw 128

section .text
_start: nop
        ; a way to inc every elem in array, a worse one than loop in terms of
        ;   mem for commads, but generated code might work faster (no checking
        ;   ecx every time), so it might be better sometimes
%assign a 0
%rep 128
        inc word [array+a]
%assign a a+2
%endrep
        FINISH
