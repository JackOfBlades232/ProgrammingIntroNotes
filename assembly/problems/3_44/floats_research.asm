%include "../useful_macros.inc"
global _start

section .data
number  dq 0.001
char    db '@'
ngchar  db '*'
nl      db 10
ln      equ 1

section .bss
dcm_ord resq 1                 

section .text
_start: finit
        fld qword [number]
        fldlg2                  
        fxch                     
        fyl2x                  
        fistp qword [dcm_ord] 
        mov ebx, [dcm_ord]
        cmp ebx, 0
        jge .lp
        neg ebx
.neg_lp:
        kernel 4, 1, ngchar, ln
        dec ebx
        test ebx, ebx
        jnz .neg_lp
        jmp .nl
.lp:    kernel 4, 1, char, ln
        dec ebx
        test ebx, ebx
        jnz .lp
.nl:    kernel 4, 1, nl, ln
.quit:  kernel 1, 0
