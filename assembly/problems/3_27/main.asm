%include "../../stud_io.inc"
global _start
extern read_num
extern write_num
extern input_str
extern output_str
extern clear_line
extern check_sign
extern apply_arifm

section .bss
digits  resb 10             ; reserve 10 bytes for number i-o
rpn     resd 1024           ; reverse polish notation stack (with limit)
lsign   resb 1              ; mem for saving off a sign

section .text
_start: cld                 ; set normal direction for rpn stack
        mov ebp, esp        ; init stack base
        mov edi, rpn        ; set edi to rpn stack top
        mov esi, edi        ; and esi to rpn stack bottom
        push dword '('      ; push initial brace in stack
read_lp:
        push dword digits   ; push param 1 (adr for digits)
        push dword 10       ; push param 2 (max num of digits)
        call input_str      ; call subpr (read digits from input)
        add esp, 8          ; clear params from stack
        push edx            ; store break symbol in stack
        cmp cl, 0           ; check if no number was read
        jz symbol           ; if so, go to deal with break symbol
        push dword digits   ; else, push adr param to stack
        push ecx            ; and push input length to stack as param
        call read_num       ; read num to int (subpr)
        add esp, 8          ; clear params from stack
        stosd               ; and put number it in rpn stack
symbol: call check_sign     ; check break sym type via subpr (alr in stack)
        add esp, 4          ; and clear param from stack
        mov bh, cl          ; also save off sign code to bh
        cmp cl, -1          ; check if invalid char
        jz err              ; if so, error
        cmp cl, 0           ; check if eof/eoln 
        jz fin_rpn          ; switch case
        cmp cl, 1           ; open bracket
        jz o_br         
        cmp cl, 2           ; close bracket
        jz fin_rpn
        cmp cl, 3           ; plus/minus
        jz pm
        mov bl, -1          ; else, it is /*, put -1 in sign code addition
        jmp sgn             ; and go to deal with sign
pm:     xor bl, bl          ; if +-, put 0 in sign code addition
        jmp sgn             ; and jump to deal with sign
o_br:   push edx            ; if open bracket, push to stack
        jmp read_lp         ; and read on
sgn:    mov ch, cl          ; if +-, store cl in ch
        mov [lsign], dl     ; and store symbol in mem (lsign)
sgn_lp: call check_sign     ; get last char in stack symbol
        add cl, bl          ; add sign code addition to cl (1 if /*)
        cmp cl, 3           ; check if sign on top is higher than +-
        jge sgn_rpn         ; if not, go to push signs to rpn
        mov dl, [lsign]     ; restore sign from mem
        push edx            ; and push it to stack
        jmp read_lp         ; and go back to read lp
sgn_rpn: 
        add esi, 8          ; check if at least 2 elems in rpn
        cmp esi, edi
        jg err              ; if esi greater than edi-1, raise error
        sub esi, 8
        push dword [edi-4]  ; else, push 2 numbers as params (sign alr there)
        push dword [edi-8]
        call apply_arifm    ; apply the calculation
        add esp, 12         ; and clear params from stack (last sign now clrd)
        cmp cl, 0           ; check if arifm ended with error
        jnz err             ; if so, raise error
        sub edi, 4          ; now, move edi pointer to first num
        mov [edi-4], eax    ; and put the result in place of first num
        jmp sgn_lp          ; and repeat sign loop
fin_rpn:
        cmp byte [esp], '(' ; check if open bracket on top of stack
        jz chk_err          ; if so, go to check error
        cmp esp, ebp        ; else, check if reached stack base
        jge err             ; if so, raise error (bracket must be at base)
        add esi, 8          ; check if at least 2 elems in rpn
        cmp esi, edi
        jg err              ; if esi greater than edi-1, raise error
        sub esi, 8
        push dword [edi-4]  ; else, push 2 numbers as params (sign alr there)
        push dword [edi-8]
        call apply_arifm    ; apply the calculation
        add esp, 12         ; and clear params from stack (last sign now clrd)
        cmp cl, 0           ; check if arifm ended with error
        jnz err             ; if so, raise error
        sub edi, 4          ; now, move edi pointer to first num
        mov [edi-4], eax    ; and put the result in place of first num
        jmp fin_rpn         ; and repeat inner loop
chk_err:    
        add esp, 4          ; remove open bracket from stack
        cmp bh, 0           ; check if stopped due to eo
        jz chk_eo           ; if so, go to check eo for error
        jmp read_lp         ; else, repeat read loop
chk_eo: add esi, 4          ; check if rpn stack also reduced to one number
        cmp edi, esi        
        jnz err             ; if not, raise error
        sub esi, 4  
        cmp esp, ebp        ; check if reached regular stack base
        jl err              ; if not, raise error
        push dword [edi-4]  ; if all good, push final number to stack as param
        push dword digits   ; and adr for digits
        call write_num      ; write the num as digits to mem (subpr)
        add esp, 8          ; clear params from stack
        push dword digits   ; again, push adr to stack
        push ecx            ; and number of digits
        call output_str     ; output number to std_io via subpr
        add esp, 8          ; and clear params from stack
        PUTCHAR 10          ; also new line after output
        jmp quit            ; and quit program
err:    dec bh              ; offset bh (so that it ll be -1(eof) if it were 0)
        shr bx, 8           ; put bh in bl
        push ebx            ; and push it to stack as param
        call clear_line     ; clear line of input
        add esp, 4          ; and clear params from stack
        PRINT "ERROR"       ; print error text
        PUTCHAR 10          ; new line
quit:   FINISH              ; and exit via macro
