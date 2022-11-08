;; 3_48/trig.asm ;;
    ; TODO : now radians are still left in stack, do so they are not
global trig
extern deg2rad
extern sincos
extern tanctan

section .text
; proc trig : gets angle in degrees and calculates all trig functions
; angle==st0 : sin->st3, cos->st2, tan->st1, ctan->st0, angle->st4
trig:   call deg2rad
        call sincos
        fild st2            ; radian angle to top for tan/ctan
        call tanctan

        ; need to drag non-radian angle to the place of radian angle
        fild st5
        fxch st5
        fstp st0

        ret
