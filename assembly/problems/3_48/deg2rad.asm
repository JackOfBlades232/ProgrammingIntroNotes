;; 3_48/deg2rad.asm ;;
global deg2rad

section .text
; proc deg2rad : converts number from float ro radians
; number==st0 : res->st0 (not popping number)
deg2rad:
        fldpi
        fmul
        fild qword 180.0
        fdivp

        ret
