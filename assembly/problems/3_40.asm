;; 3_40.asm ;;
%include "useful_macros.inc"

global _start
extern strlen

section .data
n_lines equ 10                  ; write the line 10 times
%ifdef OS_FREEBSD               
openwr_flags    equ 601h
%else   ; assume it is linux
openwr_flags    equ 241h
%endif
spc     db ' ', 0
spc_len equ $-spc
nl      db 10, 0
nl_len  equ $-nl

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
        xor ebx, ebx            ; prep outer cycle counter
.again: xor ecx, ecx            ; and prep cycle counter
.nest_lp: 
        inc ecx
        cmp ecx, [esp]
        jae .lp_fin
        strlen dword [esp+4*ecx+12]
        mov [wrd_ln], eax
        kernel 4, [fd], [esp+4*ecx+12], [wrd_ln]
        kernel 4, [fd], spc, spc_len
        jmp .nest_lp
        kernel 4, [fd], nl, nl_len
.lp_fin:
        inc ebx
        cmp ebx, n_lines
        jb .again
        kernel 6, [fd]
