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
        push edi
        mov edi, eax
        
        push dword '.'
        push eax
        call read_uns
        add esp, 8
        cmp cl, 0
        jnz .err
        mov [int_prt], eax
        fild dword [int_prt]

.skip_lp:
        inc edi
        cmp byte [edi], 0
        jnz .skip_lp
        dec edi

        fldz
        xor ecx, ecx

.frac_lp:
        mov cl, [edi]
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
        fmul st0, qword [tenth]

        jmp .frac_lp

.fin_add:
        faddp st1, st0
        xor eax, eax
        jmp .quit

.err:   mov eax, 1

.quit:  pop edi
