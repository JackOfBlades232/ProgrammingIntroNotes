%include "../../stud_io.inc"    ; macrodirective for including text

; oneline macros for defining parts of lines (here for params and vars in subp)
%define arg1 ebp+8
%define arg2 ebp+12
%define arg3 ebp+16
%define local1 ebp-8
%define local2 ebp-12
%define local3 ebp-16
; general versions
%define arg(n) ebp+(4*n)+4
%define local(n) ebp-(4*n)

; demonstration of redef and undef
%define acc eax
%define acc ecx                 ; from this point fist def is overwritten
%undef acc                      ; from this point acc is no longer defined

; macros for calling subpr with 0-3 params
%macro pcall1 2             ; 2 -- num of macro params
        push %2
        call %1
        add esp, 4
%endmacro
%macro pcall2 3             
        push %3
        push %2
        call %1
        add esp, 8
%endmacro
%macro pcall3 4             
        push %4
        push %3
        push %2
        call %1
        add esp, 12
%endmacro
%macro pcall0 1             ; for calling subpr-s with 0 params
        call %1
%endmacro

global _start

section .text
; demo subpr (print char n times and new line)
myproc:
        push ebp
        mov ebp, esp
        mov eax, [arg1]
        mov ecx, [arg2]
.lp:    PUTCHAR al
        loop .lp
        PUTCHAR 10
        mov esp, ebp
        pop ebp
        ret
; main code
_start: pcall2 myproc, '@', 10  ; one line call of subpr via macro
        FINISH
