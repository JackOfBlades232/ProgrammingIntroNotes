;; 3_36/check_one_unique.asm ;;
global check_one_unique

section .text
; proc check_one_unique : check if string consist of one unique char
; adr == [ebp+8] : result->cl (0 -- true, 1 -- false)
check_one_unique:
        push ebp                    ; init stack frame
        mov ebp, esp
        push esi                    ; will corrupt esi
        mov esi, [ebp+8]
        mov cl, 1                   ; false by default
        xor dl, dl                  ; dl for first letter
        xor al, al                  ; al for last
.lp:    cmp byte [esi], 0
        jz .true                    ; if 0 with all chars == first, ret true 
        lodsb
        test dl, dl                 ; if 0 in dl, just save first letter
        jz .save_ch
        cmp al, dl                  ; else, check if letter same as first
        jnz .quit                   ; if not, ret false instantly
        jmp .lp
.save_ch:
        mov dl, al
        jmp .lp
.true:  xor cl, cl
.quit:  pop esi
        mov esp, ebp
        pop ebp
        ret
