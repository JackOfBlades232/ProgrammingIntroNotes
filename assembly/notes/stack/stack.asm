%include "../../stud_io.inc"
global _start

section .bss
array   resb 256        ; mem for array

section .text
_start: xor eax, eax    ; zero out eax
        push eax        ; push to top of stack (can only push word or dword)
        inc eax         ; better only to use dwords
        pop eax         ; pop from stack to eax
        pushad          ; push all e-regs into stack (order: eax, ecx, edx,
                        ;   ebx, esp, ebp, esi, edi)
        popad           ; pop back to regs
        pushfd          ; push EFLAGS val to stack
        popfd           ; pop back to EFLAGS (overwrites only non-restr flags)
        jmp main        ; jump to main part
        ;   also pushaw, popaw, pushfw, popfw for 16-bit regs, but not good,
        ;       breaks stack continuity (better only to push n pop dwords)
;   subprograms: 
;   fill memory (edi=address, ecx=length, al=value)
fill_memory:
        push ebp        ; these 3 lines at beginning of each subprogram, save
        mov ebp, esp    ;   stack base in stack, mov new stack base to ebp
        sub esp, 0      ;   move esp by the mem allocated to local variables
        ;   alt command: enter 0, 0 (first zero is loc mem), but slower
        jecxz .fm_q     ; if len=0, quit
.fm_lp: mov [edi], al   ; mov val to address (. before mark to make it local)
        inc edi         ; increment address
        loop .fm_lp     ; loop length times
.fm_q:  mov esp, ebp    ; these 3 lines at end of each subpr, restore stack to
        pop ebp         ;   stack base, pop it to ebp
        ;   alt to 2 lines: leave (also slower) (but command is less in mem)
        ret             ; return control
;   pascal puts params in stack in order, so you cant change num of params 
;       (because last param is at [ebp+8]), but you can have the subprogram
;       free the params from stack itself. C puts params in reverse, so you
;       can pass unknown num of variables, but have to free stack yourself 
;       after calling the subprogram
;   proc 1 (a1, a2, a3, in stack)
proc_1: enter 0, 0      ; the other way of starting subpr
        nop             ; do nothing
        leave           ; the other way of wrapping up
        ret             ; return control
;   proc 2 (a1, a2, a3, in stack)
proc_2: push ebp        ; init seq
        mov ebp, esp    ; without local mem
        nop             ; do nothing
        mov esp, ebp    ; deinit seq
        pop ebp
        ret 12          ; return control and clear params from stack
;   main code
main:   mov edi, array  ; put array adr in edi
        mov ecx, 256    ; len in ecx
        mov al, '@'     ; val in al
        call fill_memory    ; call the subprogram
        ; call of subpr with params (C way):
        push dword 3    ; a3 to stack
        push dword 2    ; a2 to stack
        push dword 1    ; a1 to stack
        call proc_1     ; call the subpr with params
        add esp, 12     ; remove params from stack
        ; call of subpr Pascal way
        push dword 1    ; a1 to stack
        push dword 2    ; a2 to stack
        push dword 3    ; a3 to stack
        call proc_2     ; call the subpr with params, stack cleared in subpr
; stack max mem is 8mb
; ebp one above return adr, first subpr param in [ebp+8], and so on
; C and Pascal return through eax (edx:eax), except floats (through arifm co-
;   processor), and records/structs (through mem)
        FINISH          ; exit with macro
