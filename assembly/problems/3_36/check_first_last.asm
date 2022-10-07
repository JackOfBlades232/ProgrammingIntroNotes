;; 3_36/check_first_last.asm ;;
global check_first_last

section .text
; proc check_first_last : check if string starts with upc and ends with lowc
; adr == [ebp+8] : result->cl (0 -- true, 1 -- false)
check_first_last:
        push ebp                    ; init stack frame
        mov ebp, esp
        push esi                    ; will corrupt esi
        mov esi, [ebp+8]
        mov cl, 1                   ; false by default
        xor dl, dl                  ; dl for first letter
        xor al, al                  ; al for last
.lp:    cmp byte [esi], 0
        jz .decide
        lodsb
        test dl, dl                 ; if 0 in dl, save first letter
        jnz .lp
        mov dl, al
        jmp .lp
.decide:
        cmp dl, 'A'                 ; ret true only if dl is upc and al id lowc
        jb .quit
        cmp dl, 'Z'
        ja .quit
        cmp al, 'a'
        jb .quit
        cmp al, 'z'
        ja .quit
        xor cl, cl
.quit:  pop esi
        mov esp, ebp
        pop ebp
        ret
