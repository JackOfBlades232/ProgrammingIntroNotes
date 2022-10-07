;; 3_36/check_dog_n_dot.asm ;;
global check_dog_n_dot

section .text
; proc check_dog_n_dot : check if string has at least one . and precisely one @
; adr == [ebp+8] : result->cl (0 -- true, 1 -- false)
check_dog_n_dot:
        push ebp                    ; init stack frame
        mov ebp, esp
        push esi                    ; will corrupt esi
        mov esi, [ebp+8]
        mov cl, 1                   ; false by default
        xor dx, dx                  ; dl for . counting, dh for @
.lp:    cmp byte [esi], 0
        jz .decide
        lodsb
        cmp al, '.'
        jnz .cmp_dog
        inc dl
        jmp .lp
.cmp_dog:
        cmp al, '@'
        jnz .lp
        test dh, dh
        jnz .quit                   ; if more than 1 @, quit instanty
        inc dh
        jmp .lp
.decide:
        test dl, dl
        jz .quit
        test dh, dh
        jz .quit                    ; both are >0, ret true
        xor cl, cl
.quit:  pop esi
        mov esp, ebp
        pop ebp
        ret
