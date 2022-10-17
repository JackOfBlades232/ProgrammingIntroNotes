;; 3_38/write_num.asm ;;
global write_num

section .data
; constants: min and max possible digit base
mn_base equ 2
mx_base equ 35

section .text
; proc read_num : reads digits (delim by #0) to number in a given digit system 
; num == [ebp+8], adr == [ebp+12], digit_base == [ebp+16] : 
;   number->eax, success->cl
write_num:
        push ebp
        mov ebp, esp
        push edi                    ; will corrupt edi (traversing digits)
        mov eax, [ebp+8]            ; number in eax
        mov edi, [ebp+12]           ; adr in edi
        mov ecx, [ebp+16]           ; divisor in ecx
        and ecx, 0x000000ff         ; apply mask (leave cl only)
        cmp ecx, 2                  ; check if digit base between 2 and 35
        jb .err
        cmp ecx, 35
        ja .err
        xor edx, edx                ; prep edx for division
        push dword 0                ; push delimiter to stack
.cacl_lp:    
        div ecx
        cmp dl, 9
        ja .conv_A
        add edx, '0'
        jmp .push
.conv_A:
        add edx, 'A'-10
.push:  push edx
        xor edx, edx
        cmp eax, 0
        jnz .cacl_lp
.write_lp:
        pop eax
        stosb                       ; write al to  [edi] and inc edi
        cmp eax, 0                  ; check if stack reached delim
        jnz .write_lp
        xor cl, cl                  ; if loop finished on #0 put 0 in exit code
        jmp .quit
.err:   mov cl, 1                   ; if ended on err jump, put 1 in exit code
.quit:  pop esi
        mov esp, ebp 
        pop ebp
        ret
