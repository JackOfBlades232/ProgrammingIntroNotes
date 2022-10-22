;; basics.asm ;;
global _start

section .data
num1    dq 1.0                  ; initializing floats -- dd, dq or dt (4, 8, 10)
num2    dt 1.0e+3               ; dt takes only floats
num3    dd 1.0E2
num4    dq 1.0e-2               ; can also write const in hex, but not useful
wnum1   dq 1000
x       dq 12.0
y       dq 0.01
z       dq -1.44
k       dd 1

section .bss
rnum1   resd 1
rnum2   resq 1
rnum3   rest 1
rwnum1  resd 1
m       resd 1
prc_stt resb 108                ; co-proc state must be saved in 108 bytes

section .text
; all f-commands executed with co-proc, so no access to usual regs as args
_start: fld qword [num4]        ; push to co-proc register stack (no literals)
        fld st0                 ; push a copy of reg stack top
        fld st1
        fst dword [rnum1]       ; copy from stack top to somewhere (no twords)
        fstp tword [rnum3]      ; pop from stack top and makes it empty
        fstp st0                ; destroys stack top
        fild qword [wnum1]      ; write from int to float, only mem
        fist dword [rwnum1]     ; extract, same (also fistp, like fstp)
        fld1                    ; push 1 in stack (also fldz, fldpi, fldl2e, 
        fldl2t                  ;   fldln2, fldlg2)
        fxch                    ; exchanges st0 and st1
        fxch st2                ; st0 and st2
        fadd qword [num1]       ; adds mem-type operand to st0
        fadd                    ; adds st1 to st0 (also fsub - subtr, fsubr - 
        fmul                    ;   sub st0 from oper/st1, fdiv, fdivr(same))
        fdiv st2, st0           ; also 2 ops, one must be st0, res in first
        fsubp st1, st0          ; st0 must be second, this also pops st0 
        fmulp                   ; no ops = ... st1, st0
        ; RPN in action : (x + y) * (1 - z)
        fld qword [x]           ; x to stack
        fld qword [y]           ; y also
        faddp                   ; add and pop
        fld1                    ; 1 to stack
        fld qword [z]           ; z to stack
        fsubp                   ; 1 - z and pop
        fmulp                   ; final multiplication
        fiadd dword [rnum1]     ; with fi -- to pass ints as floats
        fldpi
        fabs                    ; put st0 module in st0
        fchs                    ; switch st0 sign
        frndint                 ; round st0 according to RC bits
        fsin                    ; sin(st0) to st0, also fcos, fsqrt
        fsincos                 ; inc stack, cos in st0, sin in st1
        fptan                   ; strange: tan to st1, 1 to st0
        fdivrp                  ; expl: now this command gets us cotan easily
        fld1
        fldpi
        fpatan                  ; calcs arctg(st1/st0), red stack, res in st0
        fyl2x                   ; calcs st1 * log2(st0) (calc log by any base)
        fyl2xp1                 ; st1 * log2(st0 + 1), |st0| < 1-srt(2)/2
        ; if st0 very close to 1, fyl2x will give 0, so we use fyl2xpl with x
        ; close to 0 (easily expressed), and it yields a better log, close to 0
        f2xm1                   ; calcs 2^x - 1 (-1 for all the same reasons)
        ; |x| <= 1
        fcom qword [rnum1]      ; comp st0 to mem (dword or qword), set copr flg
        fcomp st1               ; also takes st-regs, p - pop st0
        fld1
        fcompp                  ; comp st0 to st1 and pop both
        ; flags: if == : C3=1, C0=0, else C3=0, if st0<st1, C0=1, else 0
        ; if cmp between incorrect numbers (of ord ones), C2=1, else 0
        fstsw ax                ; mov f-flags to ax (must be ax, see next line)
        sahf                    ; some flags from ah to FLAGS
        ; C3->ZF, C0->CF, C2->PF, now we can use jumps
        ; min(x, y) -> m
        fld qword [y]
        fld qword [x]
        fcom                    ; simple compare of x, y
        fstsw ax                ; copy flags
        sahf
        ja .lpa                 ; if x>y, jump
        fxch                    ; else, exchange x, y
.lpa:                           ; now max in st0, min in st1
        ;fstp st0               ; remove redundant max
        ffree st0               ; can be done this way: clear st0 and inc stack
        fincstp
        fstp dword [m]          ; and pop second to m
        fld1
        ficomp dword [m]        ; cmp as int, add pop (also ficom)
        ftst                    ; cmp st0 to 0
; Exceptions:
; Invalid Operation (#I): using non-numbers as operans, log(-x), etc 
; or stack err -- pushing to full stack, using empty reg, etc, flag -- SF
; Denormalized (#D): trying op on denormalized num, or tryin to load one in
; denormalized num -- ord=0, mantis!=0, wierd shit by intel devs
; Zero Divider (#Z): div by zero
; Overflow (#O): too big (arifm, reducing tword to qword, etc)
; Underflow (#U): too small (not like #D, treats results of arifm/casting)
; Precision (#P): calc was imprecise (most of the times it is ok)
; exception flags go to first 6 bits of SR
; first 6 bits of CR tell co-proc what exceptions to ignore
; those that are not ignored yield inner interruption
; else, the proc will try to conjure upp a result (like /0 = inf)
        fclex                   ; reset SR flags
        ; interesting case: interrupt only when get to next co-proc command, so
        fimul dword [k]         ; if this yieds exception
        mov [k], ecx
        fsqrt                   ; then once it is inited k val is lost
        ; this can be healed by changing order, but to check last f-command, do
        wait                    ; this, it will check for exceptions
        fnstsw ax               ; fstsw=wait->fnstsw, cool piece
        ; same: fclex=wait+fnclex
; now to controlling CR
        ; these commands only take word-mem
        sub esp, 4
        fstcw [esp]             ; get cr, also fnstcw is w/o wait
        or word [esp], 0000110000000000b    ; set round bits to 11
        fldcw [esp]             ; load the word back to CR
        add esp, 4              
        fincstp                 ; controlling TOP -- pointer to reg stack top
        fdecstp                 ; these dont free regs! (TOP in SR)
        ; advice : use fclex before fldcw, not to trigger old exceptions
        ffree st0               ; frees st0, i/e changes TW
        fsave [prc_stt]         ; save co-proc state (and does finit auto)
        finit                   ; get co-proc in init state (also fninit)
        ; when init, CR=037Fh (round to closest, max prec, all ex masked)
        ; SR=0 => TOP=0, FIP, FDP=0 (co-proc instr and ins pointer)
        ; TW=1 -- all empty
        frstor [prc_stt]        ; restore proc from saved state
        fnop                    ; =nop
        ; for recovering/saving only CR/SR/TW we have fstenv (fnstenv), fldenv
        ; need 28 bytes of mem

        mov eax, 1
        mov ebx, 0
        int 80h
        
