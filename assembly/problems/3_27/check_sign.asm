global check_sign

section .text
; check sign type in arifm expr (sign=[ebp+8]) : sign->dl, code->cl
check_sign:
        push ebp            ; init stack frame
        mov ebp, esp
        mov dl, [ebp+8]     ; put sign in dl
        cmp dl, '('         ; check if open bracket
        jnz .chk_cl         ; if not, go to check if closed
        mov cl, 1           ; if it is, put 1 in code and quit
        jmp .quit   
.chk_cl:
        cmp dl, ')'         ; check if closed bracket
        jnz .chk_pm         ; if not, go to check if plus/minus
        mov cl, 2           ; if it is, put 2 in code and quit
        jmp .quit           
.chk_pm:
        cmp dl, '+'         ; check if plus
        jz .pm              ; if so, go to put pm code in cl
        cmp dl, '-'         ; same with -
        jz .pm
        jmp .chk_md         ; else, go to check if mul/div
.pm:    mov cl, 3           ; if plus/minus, put 3 in code and quit
        jmp .quit
.chk_md:
        cmp dl, '*'         ; check if *
        jz .md              ; if so, go to put md code in cl
        cmp dl, '/'         ; same with /
        jz .md
        jmp .chk_e          ; else, go to check if eof/eoln
.md:    mov cl, 4           ; if mul/div put 4 in code and quit
        jmp .quit
.chk_e: cmp dl, 10          ; check if eoln
        jz .e               ; if so, go to put eo code in cl
        cmp dl, -1          ; same with eof
        jz .e
        jmp .no_sgn         ; if none above, go to put no sign code in cl
.e:     xor cl, cl          ; if eo, put 0 in code and quit
        jmp .quit
.no_sgn:
        mov cl, -1          ; if none above, put -1 in cl
.quit:  mov esp, ebp        ; deinit stack frame
        pop ebp
        ret
