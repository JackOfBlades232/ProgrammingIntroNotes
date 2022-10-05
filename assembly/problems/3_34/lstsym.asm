;; 3_34/lstsym.asm ;;
global lstsym

section .text
; get last symbol of str (delim by #0) ( adr=[ebp+8] ) : last symbol->eax
lstsym:
        push ebp            
        mov ebp, esp
        mov ecx, [ebp+8]            
.lp:    cmp byte [ecx+1], 0        
        jz .fin            
        inc ecx            
        jmp short .lp     
.fin    xor eax, eax
        mov al, [ecx]               ; last char is in [ecx]
        pop ebp                     ; no local var-s, so wont touch esp
        ret
