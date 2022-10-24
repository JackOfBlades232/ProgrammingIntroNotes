;; 3_44/write_float.asm ;;
global write_float

section .data
ten     dq 10.0
tenth   dq 0.01

section .bss
number  resq 1                  ; saving off the params
address resd 1
lenght  resd 1
dcm_ord resq 1                  ; decimal order of the number
buffer  resb 53                 ; digits for decimal mantis

section .text
; proc write_float : writes a floating number as a decimal fraction to mem
; number==st0, address==eax, mem_length==ecx : digits->address, len->eax
write_float:
        ; preliminaty action: set rounding to floor and save params
        sub esp, 4
        fstcw [esp]
        or word [esp], 0000010000000000b    ; works if prev was 11, must
        fldcw [esp]
        add esp, 4
        fst qword [number]
        mov [address], eax
        mov [lenght], ecx
        ; first, calculate lg10(number), to find decimal order
        fldlg2                  ; we need 1/log10(2) in st1
        fxch                     
        ; TODO : introduce case where st0 close to one (with fyl2xp1)
        fyl2x                   ; calc log10(st0)
        fistp qword [dcm_ord]   ; floor and store the decimal order
        ; now, construct 1/10**ord
        mov ecx, [dcm_ord]
        cmp dword [dcm_ord], 0  ; order cant be too high, so dword will do
        jl .neg_ord
        fld qword [tenth]       ; if ord >= 0, we will calc 0.01**ord
.tenth_lp:
        fmul qword [tenth]
        loop .tenth_lp        
        jmp .calc_dcm_mantis
.neg_ord:
        fld qword [ten]         ; else, 10.0**(-ord)
        neg ecx
.ten_lp:
        fmul qword [ten]
        loop .ten_lp
.calc_dcm_mantis:
        ; now, multiply the number by 1/10**ord to get decimal mantis
        fld qword [number]
        fmulp                   ; multiply st0 and st1, remove them, res->st0
        ; now, we have decimal order stored and decimal mantis in st0
; testing : put ord number of @ in [eax] (not more than ecx) 
        mov ecx, [lenght]
        dec ecx
        mov edx, [dcm_ord]
        cmp ecx, edx
        jbe .write
        mov ecx, edx
        xor edx, edx
.write: mov byte [address+edx], '@'
        inc edx
        cmp edx, ecx
        jb .write
        mov byte [address+edx], 0
        mov eax, ecx
        ; quit
        ret
