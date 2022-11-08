;; 3_48/write_chars.asm ;;
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

        push ebx

        mov al, [ebp+16]
        mov ecx, [ebp+12]

        cmp ecx, bufsize
        ja .err
        
.lp:    mov [buffer+ecx-1], al
        loop .lp

        mov eax, 4
        mov ebx, [ebp+8]
        mov ecx, buffer
        mov edx, [ebp+12]
        int 80h

        xor eax, eax
        jmp .quit

.err:   mov eax, 1

.quit:  pop ebx

        mov esp, ebp
        pop ebp
        ret
