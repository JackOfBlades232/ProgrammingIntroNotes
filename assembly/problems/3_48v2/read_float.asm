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
        ; save off non-cdecl regs, no stack frame required
        push esi                    ; adr in esi (traversing digits)
        push ebx                    ; neg flag in bl
        mov esi, eax
        xor bl, bl

        ; if first sign is -, remove it and set neg flag
        cmp byte [esi], '-'
        jnz .int
        inc esi
        mov bl, 1
        
        ; first, read digits until . as an unsigned int (whole part), with subp
.int    push dword '.'
        push esi
        call read_uns
        add esp, 8
        cmp cl, 0
        jnz .err
        mov [int_prt], eax
        fild dword [int_prt]

        ; now, move esi to last digit of fraction part
.skip_lp:
        inc esi
        cmp byte [esi], 0
        jnz .skip_lp
        dec esi

        ; set fractional part accumulator in st0 to 0
        fldz
        xor ecx, ecx                ; digit to cl

        ; here, loop through digits until . (in reverse), accum frac part
.frac_lp:
        ; first, check if next (reverse) digit is . or illegal
        mov cl, [esi]
        cmp cl, '.'
        jz .fin_add
        cmp cl, '0'
        jb .err
        cmp cl, '9'
        ja .err
        sub cl, '0'                 ; conv cl to digit from char

        ; push digit to f-stack and calc (x + digit)/10
        mov [digit], cl
        fild dword [digit]
        faddp st1, st0
        fmul qword [tenth]
    
        ; now, set esi to next (rev) char and continue
        dec esi
        jmp .frac_lp

        ; if reached delim, add whole and frac part together (will be in st0)
.fin_add:
        faddp st1, st0

        ; if neg flag set, negate the result
        test bl, bl
        jz .set_ok_code
        fldz                            ; if neg, replace number with 0-number
        fxch
        fsubp st1, st0

        ; if never jumped to error, set success code in eax to 0, and go quit
.set_ok_code:
        xor eax, eax
        jmp .quit

        ; if error, 1 to code
.err:   mov eax, 1

        ; and restore all regs
.quit:  pop ebx
        pop esi
        ret
