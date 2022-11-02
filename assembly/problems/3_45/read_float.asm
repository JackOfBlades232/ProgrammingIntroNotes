;; 3_45/read_float.asm ;;
global read_float
extern read_uns

section .data
tenth   dq 0.1

section .bss
int_prt resd 1
digit   resd 1

section .text
; proc read float : reads floating point number from decimal frac (#0 delim)
; address==eax : number->st0, success_code->eax
read_float:
        push esi
        push ebx
        mov esi, eax
        xor bl, bl

        cmp byte [esi], '-'
        jnz .int
        inc esi
        mov bl, 1
        
.int    push dword '.'
        push esi
        call read_uns
        add esp, 8
        cmp cl, 0
        jnz .err
        mov [int_prt], eax
        fild dword [int_prt]

.skip_lp:
        inc esi
        cmp byte [esi], 0
        jnz .skip_lp
        dec esi

        fldz
        xor ecx, ecx

.frac_lp:
        mov cl, [esi]
        cmp cl, '.'
        jz .fin_add
        cmp cl, '0'
        jb .err
        cmp cl, '9'
        ja .err
        sub cl, '0'

        mov [digit], cl
        fild dword [digit]
        faddp st1, st0
        fmul qword [tenth]

        dec esi
        jmp .frac_lp

.fin_add:
        faddp st1, st0

        test bl, bl
        jz .set_ok_code
        fldz                            ; if neg, replace number with 0-number
        fxch
        fsubp st1, st0

.set_ok_code:
        xor eax, eax
        jmp .quit

.err:   mov eax, 1

.quit:  pop ebx
        pop esi
        ret
