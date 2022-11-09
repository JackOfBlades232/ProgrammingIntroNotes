;; 3_48/deg2rad.asm ;;
global deg2rad

section .data
hlf_cir dq 180.0

section .text
; proc deg2rad : converts number from degrees to radians
; number==st0 : res->st0 (not popping number)
deg2rad:
        fldpi
        fmul st1
        fld qword [hlf_cir]
        fdivp st1, st0

        ret
