;; 3_48/sincos.asm ;;
global sincos

section .text
; proc sincos : calcs sin and col of angle in radians without removing the angle
; number==st0 : sin->st1, cos->st0, number->st2
sincos: fild st0
        fsincos

        ret
