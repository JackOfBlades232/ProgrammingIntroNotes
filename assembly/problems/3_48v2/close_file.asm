;; 3_48/close_file.asm ;;
global close_file

section .text
; proc close file : closes file by descriptor
; descriptor==[ebp+8]
close_file:
        push ebp
        mov ebp, esp

        push ebx

        mov eax, 6
        mov ebx, [ebp+8]
        int 80h

        pop ebx

        mov esp, ebp
        pop ebp
