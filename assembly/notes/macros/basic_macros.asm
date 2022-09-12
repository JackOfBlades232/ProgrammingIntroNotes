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

; using a macro in another macro only extends it when the first macro is called
;   that means, in situation below the secon macro will be used in its second
;   definition
%define thenumber       25
%define mkvar           dd thenumber
%define thenumber       36

; here with xdefine we override lazy init for the number and make it be inser-
;   ted into the macro itself
%define othernumber    12
%xdefine mkvar2        dd othernumber
%define othernumber    24

; macrovariables: can perform arifm on other macrovariables
%assign var 25
%assign var var+1

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

; macro for calling subpr with random num of params (n-m -- from n to m params,
;   inclusive, n-* -- from n to inf)
%macro pcall 1-*            ; at least one param
  %rep %0 - 1               ; cycle through all params except first (%0=numprm)
    %rotate -1              ; rotate macro params to the left
        push dword %1       ; push the new first param (which was last)
  %endrep
  %rotate -1                ; return proc address to %1
        call %1             ; call it
        add esp, (%0 - 1) * 4   ; clear stack from params
%endmacro

global _start

section .data
var1    mkvar               ; itll be 36, as in second def of thenumber
var2    mkvar2              ; itll be 12, as in first def of othernumber

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
        pcall myproc, '*', 20   ; same, but generic version of macro
        FINISH
