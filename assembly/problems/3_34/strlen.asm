;; 3_34/strlen.asm ;;
global strlen

section .text
; calc length of str (delim by #0) ( adr=[ebp+8] ) : length->eax
strlen:
        push ebp            
        mov ebp, esp
        xor eax, eax      
        mov ecx, [ebp+8]            ; arg 1  
.lp:    cmp byte [eax+ecx], 0       ; check delimiter
        jz .quit            
        inc eax            
        jmp short .lp     
.quit:  pop ebp                     ; no local var-s, so wont touch esp
        ret
