;; 3_48/tanctan.asm ;;
global tanctan

section .text
; proc tanctan : calcs tan and ctan of number
; number==st0 : tan->st1, ctan->st0, number is popped
tanctan:
        fptan
        fdiv

        ret
