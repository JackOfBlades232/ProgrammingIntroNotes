;; 3_40.asm ;;
%include "useful_macros.inc"

global _start
extern strlen

section .data
%ifdef OS_FREEBSD               
openwr_flags    equ 601h
%else   ; assume it is linux
openwr_flags    equ 241h
%endif
spc     db ' ', 0
spc_len equ $-spc

section .bss
fd      resd 1                  ; descriptor for dest file
wrd_ln  resd 1                  ; length of arg

section .text
_start: cmp [esp], 2            ; check if file name provided
        jl .err
        mov edi, [esp+8]        ; filename into edi
        kernel 5, edi, openwr_flags     ; open file to write with syscall
        cmp eax, -1             ; check if success
        jle .err
        mov [fd], eax
        dec dword [esp]         ; remove prog name from argc
        xor ecx, ecx            ; and prep cycle counter
.again: strlen dword [esp+4*ecx+4]
        mov [wrd_ln], eax
        kernel 4, [fd], [esp+4*ecx+4], [wrd_ln]
        kernel 4, [fd], spc, spc_len
        inc ecx
        cmp ecx, [esp]
        jb .again

