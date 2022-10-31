;; 3_44/write_float.asm ;;
%include "../../stud_io.inc"
global write_float
extern write_uns

section .data
tenth   dq 0.1

section .bss
whole   resd 1

section .text
; proc write_float : writes a floating number as a decimal fraction to mem
; number==st0, address==eax, mem_length==ecx : digits->address, len->eax
        ; TODO : implement negative numbers
        ; TODO : implement ignoring trailing zeroes
write_float:
        push edi
        push esi
        push eax                    ; placeholder
        mov edi, eax
        mov esi, ecx
        dec esi
        sub esp, 4
        fstcw [esp]
        or word [esp], 0000010000000000b    ; works if prev was 11, must be 01
        fldcw [esp]
        add esp, 4
.again: fist dword [whole]
        push esi
        push edi
        push dword [whole]
        call write_uns
        add esp, 12
        add edi, ecx
        sub esi, ecx
PUTCHAR '@'
        cmp esi, 0
        jz .end
.lp:    fmul qword [tenth]
        loop .lp
        jmp .again
.end:   mov byte [edi], 0
        PUTCHAR 10
        pop eax                     ; placeholder
        pop esi
        pop edi
        ; quit
        ret
