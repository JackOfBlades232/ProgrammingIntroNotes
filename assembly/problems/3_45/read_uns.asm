;; 3_45/read_uns.asm ;;
global read_uns

section .text
; proc read unsigned : reads unsigned number from digits to int (delim)
; adr==[ebp+8], delim=[ebp+12] : number->eax, code->cl
read_uns:                  
        push ebp    
        mov ebp, esp

        push esi
        push edi
        push ebx
        mov esi, [ebp+8]
        mov ebx, [ebp+12]

        cmp byte [esi], '0'
        jb .err
        cmp byte [esi], '9'
        ja .err
        xor eax, eax
        xor ecx, ecx
        mov edi, 10

.lp:    mul edi
        mov cl, [esi]
        sub cl, '0'
        add eax, ecx

        inc [esi]
        cmp byte [esi], bl
        jz .ok
        cmp byte [esi], '0'
        jb .err
        cmp byte [esi], '9'
        ja .err

        jmp .lp
        
.ok:    xor cl, cl
        jmp .quit
.err:   mov cl, 1

.quit:  pop ebx
        pop edi
        pop esi
        mov esp, ebp 
        pop ebp
        ret
