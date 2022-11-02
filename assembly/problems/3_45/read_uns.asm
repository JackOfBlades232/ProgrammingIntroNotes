;; 3_45/read_uns.asm ;;
global read_uns

section .text
; proc read unsigned : reads unsigned number from digits to int (delim)
; adr==[ebp+8], delim=[ebp+12] : number->eax, code->cl
read_uns:                  
        ; init stack frame
        push ebp    
        mov ebp, esp

        ; prep regs and params
        push esi                    ; adr will be in esi
        push edi                    ; multiplier (10) will be in edi
        push ebx                    ; delimiter will be in bl
        mov esi, [ebp+8]
        mov ebx, [ebp+12]

        ; check first digit and prep place for accumulation
        cmp byte [esi], '0'         ; if not a single digit, term with err
        jb .err
        cmp byte [esi], '9'
        ja .err
        xor eax, eax                ; eax for accumulating number
        xor ecx, ecx                ; ecx for next digit
        mov edi, 10

        ; loop throuhg digits : x := 10*x + digit
.lp:    mul edi
        mov cl, [esi]
        sub cl, '0'
        add eax, ecx

        ; check if ran into delimiter or illegal char
        inc esi
        cmp byte [esi], bl
        jz .ok
        cmp byte [esi], '0'
        jb .err
        cmp byte [esi], '9'
        ja .err

        jmp .lp
        
        ; if reached delim, exit with code 0 (in cl)
.ok:    xor cl, cl
        jmp .quit

        ; else, with code 1
.err:   mov cl, 1

        ; restore regs and deinit stack frame
.quit:  pop ebx
        pop edi
        pop esi
        mov esp, ebp 
        pop ebp
        ret
