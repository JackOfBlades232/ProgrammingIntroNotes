%include "../../stud_io.inc"    ; macrodirective for including text
; define the symbol as oneline macro (can also add when calling assembly)
;   ( with flag -dDEBUG_PRINT )
;%define DEBUG_PRINT
%define PRINT_TWO
%define f1 FooBar
%define f2 foobar

global _start

section .text
_start: nop
; conditional compilation for debug prints
%ifdef DEBUG_PRINT
        PRINT "Entering suspicious section"
        PUTCHAR 10
%endif
        nop
%ifdef DEBUG_PRINT
        PRINT "Leaveing suspicious section"
        PUTCHAR 10
%endif
; cond comp with else
%ifdef PRINT_ONE
        PUTCHAR '1'
%elifdef PRINT_TWO
        PUTCHAR '2'
%else
        PUTCHAR '@'
%endif
        PUTCHAR 10
; cond comp when not defined ( also %elifndef )
%ifndef SHUT_UP
        PRINT "I speak!"
        PUTCHAR 10
%endif
; if for arifm conditions on constants and macros (def and assign)
%if ((1+2-123)*3 <> 28) ^^ (1 | 1 != 0)
        PUTCHAR '*'
        PUTCHAR 10
%endif
; for checking if strings are identical (as vals of macros) (..idn and ..idni)
%ifidn f1, f2
        PRINT "Identical casewise"
%elifidni f1, f2
        PRINT "Identical ignore case"
%endif
        PUTCHAR 10
; also %ifmacro for checking existance of multiline macro, %ifid, %ifstr,
;   %ifnum for checking if expr is identifier, string or number
        FINISH
