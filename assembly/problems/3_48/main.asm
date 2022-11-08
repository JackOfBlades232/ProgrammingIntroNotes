;; 3_48/main.asm ;;
%include "../useful_macros.inc"

global _start
extern calc_number_of_reps
extern trig
extern write_chars

section .bss
dgts    resb 19
dgts_ln equ $-dgts

section .data
pref    db "| "
pref_ln equ $-pref
sep     db " | "
sep_ln  equ $-sep
suf     db " |"
suf_ln  equ $-suf
line_ch db '-'
ch_ln   equ 1
line_ln equ 5*(3+dgts_ln)+1

section .text
