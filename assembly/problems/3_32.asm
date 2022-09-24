global _start

section .data
; const for required num of cli args
rq_argc equ 4                   ; 3 args required + prog name

section .text
; main code
_start: cmp byte [esp], rq_argc ; check if number of cli args is as req
        jnz .err
        xor ebx, ebx            ; if good, put 0 in exit code
        jmp .quit
.err:   mov ebx, 1              ; else, put 1 in exit code
.quit:  mov eax, 1              ; syscall exit
        int 80h
