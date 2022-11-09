;; 3_48/open_file_wr.asm ;;
global open_file_wr

section .data
openwr_flags    equ 241h
rights          equ 0666q       ; rights to rwrwrw 

section .text
; proc open wile wr : open file with given name for write with overwrite and
;   create new
; name adr==[ebp+8] : fd/err code->eax
open_file_wr:   push ebp
                mov ebp, esp

                push ebx

                mov eax, 5
                mov ebx, [ebp+8]
                mov ecx, openwr_flags
                mov edx, rights
                int 80h

                pop ebx

                mov esp, ebp
                pop ebp
                ret
