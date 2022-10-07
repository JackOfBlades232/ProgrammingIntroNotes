;; 3_35.asm ;;
%include "useful_macros.inc"

global _start
extern strlen
extern putstr

section .data
nl      db 10, 0                    ; new line string for output
nl_len  equ $-nl

section .text
; main code
_start:
        mov ebx, [esp]              ; argc to ebx
        cmp ebx, 1                  ; check if no args are passed
        jbe .quit                   ; if so, output nothing and quit
        xor ebp, ebp                ; will store max len in ebp
        mov esi, esp                ; next arg adr pointer will be in esi
        add esi, 8                  ; put it to first real arg
        dec ebx                     ; also, decrease ebx to ignore pr name
.again: pcall strlen, [esi]
        cmp eax, ebp
        jbe .inc
        mov ebp, eax
        mov edi, [esi]              ; longest adr will be in edi
.inc:   add esi, 4
        dec ebx
        cmp ebx, 0
        je .out
        jmp .again
.out:   pcall putstr, edi
        kernel 4, 1, nl, nl_len     ; and new line after
.quit:  kernel 1, 0
