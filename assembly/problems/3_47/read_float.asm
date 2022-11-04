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
; address==eax : number->st0, break char->al, empty code->ah, error code->cl
read_float:
        ; save off non-cdecl regs, no stack frame required
        push esi                    ; adr in esi (traversing digits)
        push ebx                    ; neg flag in bl
        mov esi, eax
        xor bl, bl

        ; first, check if number is empty (only break char exists)
        mov al, [esi]
        cmp al, '@'
        jz .unary_minus
        cmp al, '0'
        jb .empty
        cmp al, '9'
        ja .empty
        jmp .int

        ; if first sign is @ (code for unary -), remove it and set neg flag
.unary_minus:
        inc esi
        mov bl, 1
        
        ; first, read digits until . as an unsigned int (whole part), with subp
.int:   push dword '.'
        push esi
        call read_uns
        add esp, 8
        cmp cl, 0
        jnz .err
        mov [int_prt], eax
        fild dword [int_prt]

        ; now, move esi to last digit of fraction part (and save break char
.skip_lp:
        inc esi
        mov al, [esi]
        cmp al, '.'
        jz .skip_lp
        cmp al, '0'
        jb .reached_break
        cmp al, '9'
        ja .reached_break
        jmp .skip_lp

.reached_break:
        push eax                    ; save off break char to stack
        dec esi

        ; set fractional part accumulator in st0 to 0
        fldz
        xor eax, eax                ; digit to al

        ; here, loop through digits until . (in reverse), accum frac part
.frac_lp:
        ; first, check if next (reverse) digit is . (can't be illegal, checked)
        mov al, [esi]
        cmp al, '.'
        jz .fin_add
        sub al, '0'                 ; conv al to digit from char

        ; push digit to f-stack and calc (x + digit)/10
        mov [digit], eax
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
        fldz                        ; if neg, replace number with 0-number
        fxch
        fsubp st1, st0

        ; if not jmp to err/empty, set err code (cl) and empty code (ah) to 0
.set_ok_code:
        mov al, [esp]               ; break char was on top of stack
        add esp, 4
        xor cl, cl
        xor ah, ah
        jmp .quit

        ; if jumped to empty, set err code to 0 and empty code to 1 (br in al)
.empty: xor cl, cl
        mov ah, 1
        jmp .quit

        ; if error, 1 to error code
.err:   add esp, 4                  ; break char was on top of stack
        mov cl, 1

        ; and restore all regs
.quit:  pop ebx
        pop esi
        ret
