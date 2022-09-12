%include "../../stud_io.inc"    

; macro for filling mem with zeroes with local label
%macro zeromem 2            ; 2 params: adr and len
        ; go through stack in case call is zeromem ecx, eax
        push dword %2
        push dword %1
        pop eax
        pop ecx
; %% for macro-local labels
%%lp:   mov byte [eax], 0
        inc eax
        loop %%lp
%endmacro

%strlen sl 'some string'    ; directive for getting len of str const
%substr letter 'abcd' 4     ; directive for getting symbol from str (here, 'd')

global _start

section .bss
arr     resb 100            ; arr for testing

section .text
_start: nop
        zeromem arr, 100    ; zero out array with macro
        FINISH
