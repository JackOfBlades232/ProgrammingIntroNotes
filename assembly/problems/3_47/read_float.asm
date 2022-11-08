;; 3_45/read_float.asm ;;
%include "../useful_macros.inc"
global read_float
extern read_uns

section .data
tenth   dq 0.1

section .bss
int_prt resd 1
digit   resd 1

section .text
; proc read float : reads floating point number from decimal frac (#0 delim)
; address==eax : number->st0, break char->al, err code->ah, num_chr_read->ecx
read_float:
        ; save off non-cdeah regs, no stack frame required
        push esi                    ; adr in esi (traversing digits)
        push edi                    ; chr cnt in edi
        mov esi, eax
        mov edi, 1                  ; accounting for break char

        ; first, check if number is empty (only break char exists)
        mov al, [esi]
        cmp al, '0'
        jb .ok
        cmp al, '9'
        ja .ok
        
        ; first, read digits until . as an unsigned int (whole part), with subp
        push dword '.'
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
        inc edi
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

        ; if not jmp to err.ok, set err code (ah) and.ok code (ah) to 0
        pop eax                     ; break char was on top of stack
        
        ; if not jumped to err, set err code to 0,.ok code to 1 (br in al)
.ok:    xor ah, ah
        jmp .quit

        ; if error, 1 to error code
.err:   mov ah, 1

        ; and restore all regs
.quit:  mov ecx, edi                ; char num goes to edi
        pop edi
        pop esi
        ret
