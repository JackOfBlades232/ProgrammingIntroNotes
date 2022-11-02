;; 3_46/main.asm ;;
%include "../useful_macros.inc"

; pass value (f-reg or mem) and jump label to jump if value != 0
%macro jump_if_nonzero_value 2
        fld %1
        ftst
        fstp st0
        fstsw ax
        sahf
        jnz %2
%endmacro

; prints float to mem, params -- adr, len
%macro print_float 2
        mov eax, %1
        mov ecx, %2
        call write_float
        kernel 4, 1, %1, eax
%endmacro

global _start
extern read_float
extern write_float

section .data
two     dq 2.0
four    dq 4.0
nr_msg  db "No roots", 10
nr_ln   equ $-nr_msg
inr_msg db "Infinite roots", 10
inr_ln  equ $-inr_msg
spc     db ' '
spc_ln  equ $-spc
nl      db 10
nl_ln   equ $-nl

section .bss
dgts    resb 19
dgts_ln equ $-dgts
discr   resd 1                  ; for saving discriminant
f_root  resq 1                  ; for saving first root when printing 2

section .text
        ; quit if not 3 cli args and init float co-proc for work
_start: cmp dword [esp], 4
        jnz .err
        finit
        
        ; read 3 coeffs to f-proc from cli args, a->st2, b->st1, c->st0
%assign adr 8
%rep 3
        mov eax, [esp+adr]
        call read_float
        cmp eax, 0
        jnz .err
    %assign adr adr+4
%endrep

        ; decide what case we are working with based on coeffs eq to 0
        jump_if_nonzero_value st2, .quadratic   ; if a!=0 -- quadratic
        jump_if_nonzero_value st1, .linear      ; a=0, b!=0 -- linear
        jump_if_nonzero_value st0, .no_roots    ; a=b=0, c!=0 -- no roots
        jmp .inf_roots                          ; a=b=c=0 -- infinite roots

        ; quadratic case
.quadratic:
        ; calc discriminant (to st0, above c, b, a)
        fld st1
        fmul st2
        fld st1
        fmul st4
        fmul qword [four]
        fsubp st1, st0

        ; compare D to 0 (<0 -> no roots, =0->1 root, >0->2 roots)
        fldz
        fcomp
        fstsw ax
        sahf
        ja .no_roots
        je .calc_one_root

        ; 2 roots case: calc (-b +- sqrt(D)) / 2a, (+ in st0, - in st1)
        fsqrt                   ; D in st0 -> sqrt(D) in st0 
        fldz                    ; push 0 to st0
        fsub st3                ; sub b from 0, now -b in st0
        fsub st1                ; and sub sqrt(D), now -b-sqrt(D) in st0
        fld st1                 ; now, push sqrt(D) above to new st0 (for +)
        fsub st4                ; sub b from it, now sqrt(D)-b in st0
        fld st5                 ; now push a on top (for 2a)
        fmul qword [two]        ; a -> 2a in st0
        fdiv st2, st0           ; st2 = -b-sqrt(D) -> (-b-sqrt(D))/2a
        fdivp st1, st0          ; st1 = -b+sqrt(D) -> (-b+sqr(D))/2a and pop 2a

        ; exchange roots if st1 > st0, for ordered output, and go to pr 2 roots
        fcom
        fstsw ax
        sahf
        jae .print_two_roots
        fxch
        jmp .print_two_roots

        ; one root in quadratic case
.calc_one_root:
        ; just calc -b/2a to st0, and go to print 1 root
        fstp st0
        fldz
        fsub st2
        fld st3
        fmul qword [two]
        fdivp st1, st0
        jmp .print_one_root
    
        ; linear case
.linear:
        ; just calc -c/b and go to print 1 root
        fldz
        fsubrp st1, st0
        fdivrp st1, st0
        jmp .print_one_root

        ; if infinite roots, print message
.inf_roots:
        kernel 4, 1, inr_msg, inr_ln
        jmp .quit

        ; same with no roots
.no_roots:
        kernel 4, 1, nr_msg, nr_ln
        jmp .quit

        ; printing two roots case (actually, print one and proceed to print 
        ;   the other in one-root case)
.print_two_roots:
        ; unfortunately, write_float seems to mess up the stack, so i save off
        ;   st0 to mem and print st1, then load st0 back in
        fst qword [f_root]
        fstp st0
        print_float dgts, dgts_ln
        kernel 4, 1, spc, spc_ln    ; print space after first root
        fld qword [f_root]

        ; printing one root: all simple (with new line)
.print_one_root:
        print_float dgts, dgts_ln
        kernel 4, 1, nl, nl_ln

        ; if did not jump to error, quit with code 1
.quit:  kernel 1, 0

        ; else, with 0
.err:   kernel 1, 1
