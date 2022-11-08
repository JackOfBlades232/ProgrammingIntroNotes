;; 3_44/write_float.asm ;;
global write_float
extern write_uns

section .data
ten   dq 10.0

section .bss
number  resd 1
whole   resd 1
cr_save resb 28

section .text
; proc write_float : writes a floating number as a decimal fraction to mem
; number==st0, address==eax, mem_length==ecx : digits->address, len->eax
write_float:
        ; prep
        fstenv [cr_save]
        fld st0
        push edi                        ; nex digit adr will be in edi
        push esi                        ; mem left will be in esi
        push ebx                        ; flag "has reached ." in bl
        mov edi, eax                    ; adr to edi
        mov esi, ecx                    ; len to esi
        dec esi                         ; free space for delimiting 0
        push esi                        ; and save decremented len for later
        xor bl, bl                      ; init '.' flag
        sub esp, 4                      ; set rounding bits in CR
        fstcw [esp]
        or word [esp], 0000010000000000b    ; works if prev was 11, must be 01
        fldcw [esp]
        add esp, 4

        ; dealing with negatives
        fst dword [number]              ; extract number and check bit sign
        and dword [number], 0x80000000  ; bit sign is first
        cmp dword [number], 0
        jz .again
        fldz                            ; if neg, replace number with 0-number
        fxch
        fsubp st1, st0
        mov byte [edi], '-'             ; and write '-' to mem
        inc edi
        dec esi

        ; main cycle: take whole part, conv to int, write, mul by 10, rep
.again: fist dword [whole]              ; convert to int and store back as flt
        fild dword [whole]

        ; subtract whole part that we saved and multiply by 10
        fsubp st1, st0
        fmul qword [ten]

        ; use subpr to write integer part to mem as digits
        push esi                        
        push edi
        push dword [whole]
        call write_uns
        add esp, 12

        ; check, if integer part fit in mem (if not, delim all by 0 as err)
        test bl, bl
        jnz .upd_cnt
        cmp ecx, esi
        jnz .upd_cnt
        jmp .quit                       ; the place where it is popped

        ; update mem left, offset edi, put '.' and update flag if necessary
.upd_cnt:
        add edi, ecx
        test bl, bl
        jnz .set_esi
        mov byte [edi], '.'
        inc edi
        dec esi
        mov bl, 1
.set_esi:
        sub esi, ecx                    ; if out of mem, go to rm trail zeroes
        cmp esi, 0
        jz .quit

        ; and repeat
        jmp .again

        ; finally, put delimiting 0 and restore corrupted regs (no stack frame)
.quit:  mov byte [edi], 0
        pop eax
        fstp st0
        pop ebx
        pop esi
        pop edi
        fldenv [cr_save]
        ; quit
        ret
