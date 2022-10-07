;; 3_36/check_single_uppercase.asm ;;
global check_single_uppercase

section .text
; proc check_single_uppercase : check if string has at least one upcase letter
; adr == [ebp+8] : result->cl (0 -- true, 1 -- false)
check_single_uppercase:
        push ebp                    ; init stack frame
        mov ebp, esp
        push esi                    ; will corrupt esi
        mov esi, [ebp+8]
        mov cl, 1                   ; false by default
.lp:    cmp byte [esi], 0
        jz .quit
        lodsb
        cmp al, 'A'
        jb .lp
        cmp al, 'Z'
        ja .lp
        xor cl, cl
.quit:  pop esi
        mov esp, ebp
        pop ebp
        ret
