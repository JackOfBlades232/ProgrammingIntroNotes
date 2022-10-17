;; 3_38/read_num.asm ;;
%include "../../stud_io.inc"
global read_num

section .text
; proc read_num : reads digits (delim by #0) to number in a given digit system 
; adr == [ebp+8], digit_base == [ebp+12] : number->eax, success->cl
read_num:
        push ebp
        mov ebp, esp
        push esi                    ; will corrupt esi (traversing digits)
        mov esi, [ebp+8]            ; adr in esi
        mov ecx, [ebp+12]           ; divisor in ecx
        and ecx, 0x000000ff         ; apply mask (leave cl only)
        cmp ecx, 2                  ; check if digit base between 2 and 35
        jb .err
        cmp ecx, 35
        ja .err
        xor eax, eax 
        cmp byte [esi], 0
        jz .err                     ; at least one digit should exist
.lp:    mul ecx 
        xor edx, edx
        mov dl, [esi]
        sub dl, '0'
        cmp edx, ecx                ; check if next digit > max possible
        jae .err 
        add eax, edx 
        PUTCHAR al
        inc esi 
        cmp byte [esi], 0
        jnz .lp
        xor cl, cl                  ; if loop finished on #0 put 0 in exit code
        jmp .quit
.err:   mov cl, 1                   ; if ended on err jump, put 1 in exit code
.quit:  pop esi
        mov esp, ebp 
        pop ebp
        ret
