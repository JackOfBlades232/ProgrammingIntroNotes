;; 3_38/read_num.asm ;;
%include "conversion_macro.inc"
global read_num

section .text
; proc read_num : reads digits (delim by #0) to number in a given digit system 
; adr == [ebp+8], digit_base == [ebp+12] : number->eax, success->cl
read_num:
        push ebp
        mov ebp, esp
        push esi                    ; will corrupt esi (traversing digits)
        push ebx                    ; and ebx, with divisor
        mov esi, [ebp+8]            ; adr in esi
        mov ebx, [ebp+12]           ; divisor in ebx
        and ebx, 0x000000ff         ; apply mask (leave cl only)
        cmp ebx, 2                  ; check if digit base between 2 and 35
        jb .err
        cmp ebx, 35
        ja .err
        cmp byte [esi], 0           ; check if string is empty
        jz .err                     ; if so, error
        xor eax, eax                ; prepare eax for number
        xor ecx, ecx                ; and ecx for digit
.again: mov cl, [esi]
        cmp cl, 0                   ; check if cl is #0, if so, finish loop
        jz .ok
        char2num cl, .calc, .err    ; else, check & convert to would-be-number
        cmp cl, bl                  ; if digit exceeds max possible digit, err
        jae .err
.calc:  mul ebx                     ; multiply by digit system base
        add eax, ecx                ; and add ebx to eax
        inc esi
        jmp .again
.ok:    xor cl, cl                  ; if loop finished on #0 put 0 in exit code
        jmp .quit
.err:   mov cl, 1                   ; if ended on err jump, put 1 in exit code
.quit:  pop ebx
        pop esi
        mov esp, ebp 
        pop ebp
        ret
