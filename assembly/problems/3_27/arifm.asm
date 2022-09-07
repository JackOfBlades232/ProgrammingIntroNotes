global apply_arifm

section .text
; apply arifmetic action (sign=[ebp+16], num2=[ebp+12], num1=[ebp+8]) :
;   res->eax, error flag->cl
apply_arifm:
        push ebp            ; init stack frame
        mov ebp, esp
        mov edx, [ebp+16]   ; sign in dl
        mov ecx, [ebp+12]   ; num2 in ecx
        mov eax, [ebp+8]    ; num1 in eax
        cmp dl, '+'         ; check if plus
        jz .sum             ; if so, deal with plus (same with other signs)
        cmp dl, '-'
        jz .diff
        cmp dl, '/'
        jz .div
        cmp dl, '*'
        jz .prod
        jmp .err
.sum:   add eax, ecx        ; if sum, add ecx to eax
        jmp .good           ; go to check overflow (same with diff and prod)
.diff   sub eax, ecx
        jmp .good
.prod   mul ecx
        jmp .good
.div    xor edx, edx        ; if div, prepare edx
        cmp ecx, 0          ; check if div by 0
        jz .err             ; if so, ret error
        div ecx             ; div eax:edx by ecx
.good:  xor cl, cl          ; if good, zero out flag
        jmp .quit           ; and quit
.err:   mov cl, 1           ; if error, put 1 in error flag
.quit:  mov esp, ebp        ; deinit stack frame
        pop ebp
        ret
