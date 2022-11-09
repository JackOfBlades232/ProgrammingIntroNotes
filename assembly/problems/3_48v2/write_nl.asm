;; 3_48/write_nl.asm ;;
global write_nl

section .data
nl      db 10 
nl_ln   equ $-nl

section .text
; proc write nl : writes new line to fd
; fd==[ebp+8]
write_nl:
        push ebp
        mov ebp, esp
        
        push ebx

        mov eax, 4
        mov ebx, [ebp+8]
        mov ecx, nl
        mov edx, nl_ln
        int 80h

        pop ebx

        mov esp, ebp
        pop ebp
        ret
