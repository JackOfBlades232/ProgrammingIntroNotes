;; 3_48/write_string ;;
global write_string

section .text
; proc write string : output string to fd from adr with given length
; adr==[ebp+16], len=[ebp+12], fd=[ebp+8] : string->fd
write_string:
        push ebp
        mov ebp, esp
        
        push ebx

        mov eax, 4
        mov ebx, [ebp+8]
        mov ecx, [ebp+16]
        mov edx, [ebp+12]
        int 80h

        pop ebx

        mov esp, ebp
        pop ebp
        ret
