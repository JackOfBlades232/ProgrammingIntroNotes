;; 3_48/trig.asm ;;
global trig
extern deg2rad

section .bss
radians resq 1

section .text
; proc trig : gets angle in degrees and calculates all trig functions
; angle==st0 : sin->st3, cos->st2, tan->st1, ctan->st0, angle->st4
trig:   call deg2rad
        fist qword [radians]

        ; sin/cos
        fsincos

        ; tan/ctan
        fld qword [radians]     ; radian angle to top for tan/ctan
        fptan
        fdiv st1, st0

        ret
