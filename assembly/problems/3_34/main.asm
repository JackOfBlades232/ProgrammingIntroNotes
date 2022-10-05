;; 3_34/main.asm ;;
%include "../useful_macros.inc"

global _start
%ifndef EQU_LSTSYM                  
  extern strlen
%endif
%ifndef EQU_LENGTH
  extern lstsym
%endif

section .data
; constants
rq_argc equ 3

section .text
; main code
_start: mov ebx, [esp]              ; retrieve argc
        cmp ebx, rq_argc            ; if not req num of args, raise error
        jnz .err                     
        mov esi, esp                ; adr in esi, put to first arg
        sub ebx, 2                  ; remove name and 1st arg from argc
        add esi, 8                  ; and put esi to first non-name arg
%ifndef EQU_LSTSYM                  ; if definitions, only do one requirement
        pcall strlen, [esi]
        mov edi, eax                ; save first arg len in edi
%endif
%ifndef EQU_LENGTH
        pcall lstsym, [esi]
        mov ebp, eax                ; save fisrt arg last symbol to ebp
%endif
.again: cmp ebx, 0                  ; compare every arg len to len(arg1)
        jle .ok                     ; if out of args, return 0 as exit code
        add esi, 4
%ifndef EQU_LSTSYM 
        pcall strlen, [esi]
        cmp edi, eax
        jnz .err                    ; if lens dont match, return error code
%endif
%ifndef EQU_LENGTH
        pcall lstsym, [esi]
        cmp ebp, eax
        jnz .err
%endif
        dec ebx
        jmp .again
.ok:    xor ebx, ebx
        jmp .quit
.err:   mov ebx, 1
.quit:  kernel 1, ebx
