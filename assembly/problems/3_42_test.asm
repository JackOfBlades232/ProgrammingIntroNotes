;; 3_42_test.asm ;;
%include "useful_macros.inc"
%include "debug_tools.inc"

global _start

section .data
f_nm    db "result.dat", 0
dbg_msg db "@ ", 0
dbg_ln  equ $-dbg_msg
nl      db 10, 0
nl_ln   equ $-nl

section .bss
fd      resd 1
ln_cnt  resd 1
chr_cnt resd 1
max_ln  resd 1

section .text
_start: kernel 5, f_nm, 0
        cmp eax, -1
        jz .err
        mov [fd], eax
        kernel 3, [fd], ln_cnt, 4
        cmp eax, 4
        jl .err
        kernel 3, [fd], chr_cnt, 4
        cmp eax, 4
        jl .err
        kernel 3, [fd], max_ln, 4
        cmp eax, 4
        jl .err
        kernel 6, [fd]
        debug_print_number [ln_cnt], dbg_msg, dbg_ln, nl, nl_ln
        debug_print_number [chr_cnt], dbg_msg, dbg_ln, nl, nl_ln
        debug_print_number [max_ln], dbg_msg, dbg_ln, nl, nl_ln
        kernel 1, 0
.err:   kernel 1, 1
