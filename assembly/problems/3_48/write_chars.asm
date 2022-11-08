;; 3_48/write_chars.asm ;;
%include "../useful_macros.inc"
global write_chars

section .bss
buffer  resb 1024
bufsize equ $-buffer

section .text
; proc write chars : outputs n(<=1024) consecutive chars to file descriptor
; char==[ebp+16], n==[ebp+12], fd=[ebp+8] : chars->file, code->eax
write_chars:
        push ebp
        mov ebp, esp

        mov al, [ebp+16]
        mov ecx, [ebp+12]

        cmp ecx, bufsize
        jg .err
        
.lp:    mov [buffer+ecx-1], al
        loop .lp

        kernel 3, [ebp+8], buffer, [ebp+12]
        xor eax, eax
        jmp .quit

.err:   mov eax, 1

.quit:  mov esp, ebp
        pop ebp
        ret
