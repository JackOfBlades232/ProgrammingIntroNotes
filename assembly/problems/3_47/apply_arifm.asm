;; 3_47/apply_arifm.asm ;;
global apply_arifm

section .text
; proc apply arifm : takes sign, and does st1 'sign' st0, pop both and store res
; num1==st1, num2==st0, sign==al : res->st0, code->cl
apply_arifm:
        cmp al, '+'
        jnz .chk_minus
        faddp st1, st0
        jmp .ok

.chk_minus:
        cmp al, '-'
        jnz .chk_prod
        fsubp st1, st0
        jmp .ok

.chk_prod:
        cmp al, '*'
        jnz .chk_div
        fmulp st1, st0
        jmp .ok

.chk_div:
        cmp al, '/'
        jnz .err
        fldz                    ; with division, raise error if dividing by 0
        fcomp
        fstsw ax
        sahf
        jz .err
        fdivp st1, st0

.ok:    xor cl, cl
        jmp .quit
        
.err:   mov cl, 1

.quit:  ret
