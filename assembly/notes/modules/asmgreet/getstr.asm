;; asmgreet/getstr.asm ;;
%include "kernel.inc"
global getstr
section .text
; proc getstr
; [ebp+8] == buffer address, [ebp+12] == buffer length
; does not eat up input til eoln
getstr:
        push ebp
        mov ebp, esp
        xor ecx, ecx                ; ecx for counting read symbols
        mov edx, [ebp+8]            ; current buffer address in ebx
.again: inc ecx                     ; increment counter right away
        cmp ecx, [ebp+12]           ; and compare to buf size (quit before #0)
        jae .quit
        push ecx                    ; saving ecx and edx from corruption
        push edx
        kernel 3, 0, edx, 1         ; syscall read from stdin for 1 byte
        pop edx                     ; restore regs
        pop ecx
        cmp eax, 1                  ; did syscall return 1 (1 byte read)?
        jne .quit                   ; if not, quit (out of input)
        mov al, [edx]               ; get read symbol in al
        cmp al, 10                  ; if new line, quit
        je .quit
        inc edx                     ; increment current address and repeat
        jmp .again
.quit:  mov [edx], byte 0           ; cap off buffer with #0
        mov esp, ebp
        pop ebp
        ret
