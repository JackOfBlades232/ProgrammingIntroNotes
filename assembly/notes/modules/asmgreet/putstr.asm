;; asmgreet/putstr.asm ;;
%include "kernel.inc"
global putstr
extern strlen                       ; need for len in syscall
section .text
; proc putstr
; [ebp+8] == address of the string
putstr: push ebp
        mov ebp, esp
        push dword [ebp+8]          ; push adr param for strlen
        call strlen
        add esp, 4                  ; len now in eax
        kernel 4, 1, [ebp+8], eax   ; write syscall
        mov esp, ebp
        pop ebp
        ret
