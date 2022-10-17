;; 3_39.asm ;;
%include "useful_macros.inc"

global _start
extern write_hex
extern putstr

section .data
nl      db 10, 0
nl_len  equ $-nl

section .bss
buffer  resb 4096                   ; buffer for reading
bufsize equ $-buffer
fd      resd 1                      ; source file descriptor
num_dgs resb 9

section .text
_start: cmp dword [esp], 2          ; check if name+1arg was passed
        jnz .err
        mov edi, [esp+8]            ; if all ok, filename to edi
        kernel 5, edi, 0            ; open syscall, O_RDONLY
        cmp eax, -1                 ; check if couldn't open
        jz .err
        mov [fd], eax
        xor ebx, ebx                ; ebx for nl counter
.again: kernel 3, [fd], buffer, bufsize     ; read syscall, from file to buf        
        cmp eax, 0                  ; check if eof or error
        jle .quit
        mov ecx, eax                ; and now count new lines in buffer
.chk_loop:
        cmp byte [buffer+ecx-1], 10 
        jnz .rep
        inc ebx
.rep    loop .chk_loop
        jmp .again
.quit:  pcall write_hex, num_dgs, ebx
        pcall putstr, num_dgs
        kernel 4, 1, nl, nl_len
        kernel 6, [fd]              ; close file
        kernel 1, 0                 ; and exit w/code 0
.err:   kernel 1, 1                 ; if error, with 1
