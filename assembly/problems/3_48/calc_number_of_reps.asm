;; 3_48/calc_number_of_reps.asm ;;
global calc_number_of_reps

section .bss
steps   resd 1
cr_save resb 28

section .text
; proc calc number of reps: given min angle, max angle, step, calcs number of
;   steps from min to max (or gives error code), and pops max from stack
; step==st2, min==st1, max==st0 : #steps/errcode->eax, step->st1, min->st0
calc_number_of_reps:
        fcom st1
        fstsw ax
        sahf
        jb .err
        fldz
        fcomp st3
        fstsw ax
        sahf
        jae .err

        ; set rounding to smaller number (temporary)
        fstenv [cr_save]
        sub esp, 4
        fstcw [esp]
        or word [esp],  0000010000000000b
        fldcw [esp]             
        add esp, 4
        
        fsub st1
        fdiv st2
        fistp dword [steps]

        mov eax, [steps]
        inc eax                 ; we rounded to lower, add one arbitr rep

        fldenv [cr_save]

        jmp .quit

.err:   xor eax, eax            ; 0 reps is err

.quit:  ret
