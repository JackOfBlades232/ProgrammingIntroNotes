;; 3_36/main.asm ;;
%include "../useful_macros.inc"

global _start
extern putstr

%ifdef A
  extern check_single_uppercase
%elifdef B
  extern check_dog_n_dot
%elifdef C
  extern check_first_last
%elifdef D
  extern check_one_unique
%else
  %error "Specify problem subsection (A-D)"
%endif

section .data
nl      db 10, 0                    ; new line string for output
nl_len  equ $-nl

section .text
; main code
_start:
        mov ebx, [esp]              ; argc to ebx
        cmp ebx, 1                  ; check if no args are passed
        jbe .quit                   ; if so, output nothing and quit
        mov esi, esp                ; next arg adr pointer will be in esi
        add esi, 8                  ; put it to first real arg
        dec ebx                     ; also, decrease ebx to ignore pr name
.again: 
%ifdef A
        pcall check_single_uppercase, [esi]
%elifdef B
        pcall check_dog_n_dot, [esi]
%elifdef C
        pcall check_first_last, [esi]
%elifdef D
        pcall check_one_unique, [esi]
%endif
        test cl, cl                 ; check if 0 was returned
        jnz .inc                    ; if false, do nothing
        pcall putstr, [esi]         ; else, output arg
        kernel 4, 1, nl, nl_len     ; and new line
.inc:   add esi, 4
        dec ebx
        cmp ebx, 0
        jnz .again
.quit:  kernel 1, 0
