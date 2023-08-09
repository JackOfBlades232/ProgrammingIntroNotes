;; read_hex.asm ;;
global read_hex

section .text
; proc read_hex : read hex bumber from digits in mem to number
; adr == [ebp+8] : number->eax, break char->cl
read_hex:
        push ebp
        mov ebp, esp
        push esi                ; will corrupt esi
        mov esi, [ebp+8]        ; adr to esi
        xor eax, eax            ; prepare eax for number
        xor ecx, ecx            ; prepare ecx for storing digits
.again: mov cl, [esi]
        cmp cl, '0'             ; check if correct digit
        jb .quit
        cmp cl, '9'
        ja .check_letter
        sub cl, '0'             
        jmp .mul
.check_letter:
        cmp cl, 'a'
        jb .quit
        cmp cl, 'f'
        ja .quit
        sub cl, 'a'-10
.mul:   shl eax, 4              ; multiply eax by 16
        add eax, ecx            ; and add ecx to eax
        inc esi
        jmp .again
.quit:  pop esi
        mov esp, ebp
        pop ebp
        ret
