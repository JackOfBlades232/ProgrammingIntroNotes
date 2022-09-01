%include "../../stud_io.inc"
global _start

section .text
_start: mov eax, \
        ecx                 ; how to break lines

        ; empty line is ignored, comment line is not
_$#@~.?:                    ; allowed symbols in labels, first only _, ?, .
        nop
        ; labels are register-sensitive
        MOV eax, 1          ; command names are case-insensitive
        PUTCHAR 10          ; and macro names are sensitive

