global _start

section .data
msg     db "Hello world!", 10
msg_len equ $-msg

section .text
_start: mov eax, 4          ; 4 in eax for syscall write (it's code)
        mov ebx, 1          ; params are passed through ebx, ecx, edx, esi, edi
        mov ecx, msg        ;   and rarely ebp, for write params are:
        mov edx, msg_len    ; 1) outp type code 2) value adr 3) byte length
        int 80h             ; make syscall, 80h is interr code for syscalss

        cmp eax, 0xfffff000 ; check if exit code is error
        jnb quit            ; if between fffff000h and ffffffffh then leave code
        xor eax, eax        ; else, zero it out

quit:   mov ebx, eax        ; result code (only param, will use write code)
        mov eax, 1          ; code 1 is _exit syscall
        int 80h             ; syscall again
