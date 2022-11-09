;; 3_48/trig.asm ;;
global trig
extern deg2rad

section .bss
radians resq 1

section .text
; proc trig : gets angle in degrees and calculates all trig functions
; angle==st0 : sin->st0, cos->st1, tan->st2, ctan->st3, angle->st4
trig:   call deg2rad
        fst qword [radians]

        ; tan/ctan
        fptan
        fdiv st1
        fxch

        ; sin/cos
        fld qword [radians]     ; radian angle to top for sin/cos
        fsincos
        fxch

        ret
