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
spc     db ' '
spc_len equ $-spc
nl      db 10
nl_len  equ $-nl

section .bss
fd      resd 1                  ; descriptor for dest file
wrd_ln  resd 1                  ; length of arg

section .text
_start: cmp dword [esp], 2      ; check if file name provided
        jl .err
        mov edi, [esp+8]        ; filename into edi
        kernel 5, edi, openwr_flags     ; open file to write with syscall
        cmp eax, -1             ; check if success
        jle .err
        mov [fd], eax
        sub dword [esp], 2      ; remove prog and file name from argc
        xor ebx, ebx            ; prep outer cycle counter
.again: xor esi, esi            ; and prep cycle counter
.nest_lp: 
        inc esi
        cmp esi, [esp]
        ja .lp_fin
        mov edi, [esp+4*esi+8]  ; next word adr in edi
        pcall strlen, edi
        mov [wrd_ln], eax
        kernel 4, [fd], edi, [wrd_ln]
        kernel 4, [fd], spc, spc_len
        jmp .nest_lp
.lp_fin:
        kernel 4, [fd], nl, nl_len
        inc ebx
        cmp ebx, n_lines
        jb .again
        kernel 6, [fd]
        kernel 1, 0             ; if not jumped to err, exit with code 0
.err:   kernel 1, 1             ; else, code 1
