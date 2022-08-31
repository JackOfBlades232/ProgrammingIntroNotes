%include "../../stud_io.inc"
global _start
extern read_num
extern write_num
extern input_str
extern output_str
extern check_char
extern apply_arifm
extern eat_input

section .bss
digits  resb 10             ; array of digits for i-o

section .text
; main code
_start: ; PRINT "This calc does not support numbers longer than 10 digits, "
        ; PRINT "negative numbers and overflow exceptions"
        ; PUTCHAR 10          ; output disclaimer
        mov ebp, esp        ; init stack base
        sub ebp, 4          
main_lp:
        push dword digits   ; put first param for subpr
        push dword 10       ; push second param for subpr
        call input_str      ; call subprogram
        add esp, 8          ; clear params from stack
        cmp dl, -2          ; check if overflow when input
        jz err              ; if so, raise error
        mov bl, dl          ; put break symbol in bl
        cmp cl, 0           ; check if no number was read
        jz brk_symbol       ; if so, deal with break symbol
        push eax            ; else, push first param (adr)
        push ecx            ; and second param (len)
        call read_num       ; call subpr
        add esp, 8          ; clear params from stack
        push eax            ; and push number to stack
brk_symbol:
        cmp bl, ' '         ; check if space
        jz main_lp          ; if so, repeat loop
        cmp bl, -1          ; check if eof
        jz arifm            ; if so, go to loop fin
        cmp bl, 10          ; check if eoln, if so go to loop fin
        jz arifm            ; if so, also go to loop fin
        cmp bl, ')'         ; check if it is a closing parens
        jz arifm            ; if so, go to lp fin
        push ebx            ; put char in stack (first param)
        call check_char     ; call subpr
        add esp, 4          ; clear param from stack
        cmp cl, 0           ; check if the char is good
        jnz err             ; if it is not good, go to error
        push ebx            ; if all good, push char to stack
        jmp main_lp         ; and repeat main loop
arifm:  mov esi, esp        ; put stack top in esi
backtrack_lp:
        cmp ebp, esi        ; check if reached stack base
        jz arifm_lp_init    ; if so, go to arifm loop
        cmp byte [esi+4], '('   ; check if sign is open bracket
        jz arifm_lp_init    ; same here
        add esi, 4          ; inc esi by dword
        jmp backtrack_lp    ; and repeat loop
arifm_lp_init:
        mov eax, [esi]      ; put first number in accumulator
        mov edi, esi        ; copy esi to edi
arifm_lp:
        cmp edi, esp        ; check if reached stack top
        jle arifm_fin       ; if so, stop arifm
        sub edi, 8          ; move edi up by 2 places
        push dword [edi+4]  ; push sign param
        push dword [edi]    ; push second num
        push eax            ; put num1 param in stack
        call apply_arifm    ; other params already in stack
        add esp, 12         ; clear params from stack
        cmp cl, 0           ; check if err
        jnz err             ; if so, jump to err
        jmp arifm_lp        ; repeat until finished arifm chain
arifm_fin: 
        mov esp, esi        ; put stack top at first number
        cmp ebp, esp        ; check if reached stack base
        jz result           ; if so, go to res
        add esp, 4          ; else, offset stack top
        mov [esp], eax      ; push final number
        jmp main_lp         ; and repeat main loop
result:
        mov [esp], eax      ; push final number
        push dword digits   ; push adr param to stack 
        call write_num      ; call subpr
        add esp, 8          ; clear params from stack
        push dword digits   ; push adr param to stack
        push ecx            ; push len to stack
        call output_str     ; call subpr
        add esp, 8          ; clear stack
        jmp quit            ; jump to quit
err:    push ebx            ; push last read char (param) to stack
        call eat_input      ; call subpr to eat up line
        add esp, 4          ; clear params from stack
        PRINT "ERROR"       ; print message
quit:   PUTCHAR 10          ; new line
        FINISH              ; exit with macro
