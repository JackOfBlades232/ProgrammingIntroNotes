;; 3_37/read_oct.asm ;;
global read_oct

section .text
; proc read_oct : read oct bumber from digits in mem to number
; adr == [ebp+8] : number->eax, break char->cl
read_oct:
        push ebp
        mov ebp, esp
        push esi                ; will corrupt esi
        mov esi, [ebp+8]        ; adr to esi
        xor eax, eax            ; prepare eax for number
        xor ecx, ecx            ; prepare ecx for storing digits
.again: mov cl, [esi]
        cmp cl, '0'             ; check if correct digit
        jb .quit
        cmp cl, '7'
        ja .quit
        sub cl, '0'             ; if digit, convert to number
        shl eax, 3              ; multiply eax by 8
        add eax, ecx            ; and add ecx to eax
        inc esi
        jmp .again
.quit:  pop esi
        mov esp, ebp
        pop ebp
        ret
