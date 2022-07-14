%include "../../stud_io.inc"
global _start

section .bss
array   resb 128        ; 128 byte array

section .text
_start: mov ecx, 128    ; elem count -> ECX
        mov edi, array  ; array adress -> EDI 
        mov al, '@'     ; char -> one-byte AL  
again:  mov [edi], al   ; move char to array elem
        PUTCHAR [edi]   ; print out array elem with macro
        inc edi         ; increment adress
        dec ecx         ; decrease left elem count
        jnz again       ; if still not 0, repeat
        PUTCHAR 10      ; new line with macro
        FINISH          ; exit
