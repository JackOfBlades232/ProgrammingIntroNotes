%include "../../stud_io.inc"
global _start
extern read_num
extern write_num
extern input_str
extern output_str

section .bss
digits  resb 10             ; array of digits for i-o

section .text
; main code
_start: xor bl, bl          ; prepare read counter
input:  push dword digits   ; push param adr
        push dword 10       ; push param len
        call input_str      ; call subpr 
        add esp, 8          ; clear stack from 2 params
        push eax            ; push adr param to stack
        push ecx            ; push length param to stack
        call read_num       ; call subpr (all params in place)
        add esp, 8          ; clear stack from 2 params
        cmp cl, 0           ; check if exit code was 0
        jnz of              ; if it wasnt, quit with overflow warning
        push eax            ; push first number to stack
        inc bl              ; increment read counter
        cmp bl, 2           ; check if read 2 numbers
        jl input            ; if not, read another number
        xor ebp, ebp        ; use ebp as output num counter
        pop ebx             ; pop second number to ebx
c_lp:   mov eax, [esp]      ; put first number in eax
        cmp ebp, 0          ; check if first iter
        jz c_sum            ; if so, go to calc sum
        cmp ebp, 1          ; check if second iter
        jz c_diff           ; if so, go to calc diff
        mul ebx             ; else, calc prod
        jmp of_chk          ; and go to output
c_diff: sub eax, ebx        ; if diff, calc diff
        jmp of_chk          ; and jump to output
c_sum:  add eax, ebx        ; else, add ebx to eax
of_chk: jc of               ; if carry flag, go to overflow
        push eax            ; push number param
        push dword digits   ; push adr param 
        call write_num      ; call subpr
        add esp, 8          ; clear stack from 2 params
        push dword digits   ; init adr param (for output, length in cl already)
        push ecx            ; push param len
        call output_str     ; call subpr
        add esp, 8          ; clear stack from 2 params
        PUTCHAR ' '         ; put space after
c_lp_fin:
        inc ebp             ; increment iter counter
        cmp ebp, 3          ; check if all iterations done
        jl c_lp             ; if not, go to calc loop start
        PUTCHAR 10          ; newline after output
        jmp quit            ; and quit
of:     PRINT "OVERFLOW"    ; warn about overflow
        PUTCHAR 10          ; and new line
quit:   FINISH              ; exit with macro
