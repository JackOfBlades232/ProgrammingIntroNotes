;; 3_47/main.asm ;;
        ; TODO : implement unary minuses transformation
        ; TODO : implement space ignoring
        ; TODO : implement error handling (zero div (fl err), empty braces)
        ; TODO : refactor
%include "../useful_macros.inc"

global _start
extern read_float
extern write_float
extern calc_sgn_weight
extern apply_arifm

%macro store_sgn 1                  ; param -- literal char, mem or reg
        mov byte [edi], %1
        inc edi
%endmacro

%macro jump_if_sgn_stack_empty 2    ; params -- stack bottom and and jmp label
        cmp edi, %1
        jbe %2
%endmacro

%macro jump_if_sgn_stack_not_empty 2 ; params: stack bottom and and jmp label
        cmp edi, %1
        ja %2
%endmacro

%macro apply_rpn_stack_operation 3  ; params: stack bttom, rpn height, err label
        cmp byte %2, 2
        jl %3
        call apply_arifm
        cmp cl, 0
        jnz %3
        dec byte %2
        dec edi
        jump_if_sgn_stack_empty %1, %3
%endmacro

section .data
nl      db 10
nl_ln   equ $-nl
err_msg db "Invalid expression in input! Max 1024 chars, floats with .", 10
err_ln  equ $-err_msg
stk_err db "Max stack depth for calculations is 8", 10
stk_eln equ $-stk_err

section .bss
res     resb 19                 ; for result digits
res_ln  equ $-res
exp_buf resb 1024               ; buffer for chars in expression
buf_ln  equ $-exp_buf-1         ; leave 1 for #0
exp_ln  resd 1                  ; length will be determined after syscall
sg_stk  resb 256                ; stack for Dijkstra alg sign stack
rpn_h   resb 1                  ; current height of rpn stack 

section .text
        ; first, preparations
_start: finit

        ; now, read input to expression buffer
        kernel 3, 0, exp_buf, buf_ln
        cmp eax, 0
        jle .err
        mov [exp_ln], eax
        mov byte [exp_buf+eax-1], 0   ; put delim #0 instead of #10

        ; and perform Dijkstra alg, with stN stack representing the rpn stack
        mov esi, exp_buf        ; will traverse expr with esi
        mov edi, sg_stk         ; and sign stack with edi
        store_sgn '('           ; need to push open bracket initially
        mov byte [rpn_h], 0     ; init rpn stack height (empty)

.again: 
        ; first, read number (can be empty) with 1 break char
        mov eax, esi
        call read_float
        cmp ah, 0               ; check if error when reading
        jnz .err
        cmp ecx, 1
        jz .offset_adr
        inc byte [rpn_h]
        cmp byte [rpn_h], 8
        jge .stk_err
.offset_adr:
        add esi, ecx            ; offset esi by number of chars read

        ; now, number in st0 (if non-empty), break char in al, esi is offset
        ;   and we need to classify break char and do something with it
        call calc_sgn_weight    ; get weigth of char in al into cl
        ;   #0 : 0, '(': 1, '+','-': 2, '*','/': 3, ')': 4, else: -1
        push eax                ; we know, that the above proc leaves al
        push ecx
        cmp cl, -1
        jz .err
        cmp cl, 0
        jz .empty_stack
        cmp cl, 1
        jz .push_to_stack
        cmp cl, 3
        jbe .decide_sgn
        cmp cl, 4
        jz .empty_stack
        
        ; if sign is +-/*, check top of sign stack to decide, what to do
.decide_sgn:
        mov al, [edi-1]
        call calc_sgn_weight
        cmp cl, [esp]
        jb .push_to_stack
        apply_rpn_stack_operation sg_stk, [rpn_h], .err
        jmp .decide_sgn

        ; if the sign is to be pushed to sign stack, just do that
.push_to_stack:
        add esp, 4
        pop eax
        store_sgn al
        jmp .again

.empty_stack:
        mov al, [edi-1]
        cmp al, '('
        jz .fin_empty
        apply_rpn_stack_operation sg_stk, [rpn_h], .err
        jmp .empty_stack

.fin_empty:
        dec edi
        pop ecx
        cmp cl, 0
        jz .expr_fin
        cmp cl, 4
        jz .close_br
        jmp .err

.close_br:
        jump_if_sgn_stack_empty sg_stk, .err
        jmp .again

.expr_fin:
        jump_if_sgn_stack_not_empty sg_stk, .err
        cmp byte [rpn_h], 1
        jg .err

        mov eax, res
        mov ecx, res_ln
        call write_float

        kernel 4, 1, res, eax
        kernel 4, 1, nl, nl_ln

        kernel 1, 0

.err:   kernel 4, 1, err_msg, err_ln
        jmp .err_quit

.stk_err:
        kernel 4, 1, stk_err, stk_eln

.err_quit:
        kernel 1, 1
