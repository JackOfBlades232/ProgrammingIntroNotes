global check_char

section .text
; check if char is sign or open parens (char=[ebp+8]) : code->cl
check_char:
        push ebp            ; init stack frame
        mov ebp, esp
        push ebx            ; save ebx
        mov bl, [ebp+8]     ; put char in bl
        cmp bl, '+'         ; check if +
        jz .sumdiff         ; if so, ret sumdiff code (same for others)
        cmp bl, '-'
        jz .sumdiff
        cmp bl, '/'
        jz .muldiv
        cmp bl, '*'
        jz .muldiv
        cmp bl, '('
        jz .paren
        mov cl, 3           ; if none of above, put 3 in exit code
        jmp .quit           ; and quit subpr
.sumdiff:
        xor cl, cl          ; sumdiff code is 0
        jmp .quit
.muldiv:
        mov cl, 1           ; muldiv code is 1
        jmp .quit
.paren: mov cl, 2           ; paren code is 2
.quit:  pop ebx             ; restore ebx
        mov esp, ebp        ; and deinit stack frame
        pop ebp
        ret
