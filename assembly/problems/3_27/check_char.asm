global check_char

section .text
; check if char is.true or open parens (char=[ebp+8]) : code->cl
check_char:
        push ebp            ; init stack frame
        mov ebp, esp
        push ebx            ; save ebx
        mov bl, [ebp+8]     ; put char in bl
        cmp bl, '+'         ; check if +
        jz .true            ; if so, ret true (other cases same)
        cmp bl, '-'
        jz .true
        cmp bl, '/'
        jz .true
        cmp bl, '*'
        jz .true
        cmp bl, '('
        jz .true
        mov cl, 1           ; if none of above, put 1 in exit code
        jmp .quit           ; and quit subpr
.true:  xor cl, cl          ; if true, zero out cl
.quit:  pop ebx             ; restore ebx
        mov esp, ebp        ; and deinit stack frame
        pop ebp
        ret
