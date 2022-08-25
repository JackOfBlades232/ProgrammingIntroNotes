%include "../../stud_io.inc"
global _start
extern read_num
extern write_num
extern input_str
extern output_str

section .bss
in_str  resb 10             ; 10 bytes for reading input
out_str resb 10             ; 10 bytes for output of functions

section .text
; main code
_start: push dword in_str   ; push param adr
        push dword 10       ; push param len
        call input_str      ; call subpr 
        add esp, 8          ; clear stack from 2 params
        push eax            ; push adr param to stack
        push ecx            ; push length param to stack
        call read_num       ; call subpr (all params in place)
        add esp, 8          ; clear stack from 2 params
        cmp cl, 0           ; check if exit code was 0
        jnz quit            ; if it wasnt, quit program
        push eax            ; push number param
        push dword out_str  ; push adr param 
        call write_num      ; call subpr
        add esp, 8          ; clear stack from 2 params
        push dword out_str  ; push param adr
        push ecx            ; push param len
        call output_str     ; call subpr
        add esp, 8          ; clear stack from 2 params
quit:   FINISH              ; exit with macro
