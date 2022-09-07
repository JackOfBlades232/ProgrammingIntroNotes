%include "../../stud_io.inc"
global read_num

section .text
; read unsigned number (address=[ebp+12], length=[ebp+8]) :
;   number->eax, success flag->cl
; overflow raises error
read_num:                   ; SUBPR START
        push ebp            ; init stack frame
        mov ebp, esp
        push esi            ; save prev esi value, will be using it
        push ebx            ; also save ebx
        mov ecx, [ebp+8]    ; put length param in ecx
        mov esi, [ebp+12]   ; put adr param in esi
        cmp cl, 0           ; check if length is 0
        jz .err             ; if so, go to err
        xor eax, eax        ; zero out number
        mov ebx, 10         ; place multiplier in ebx
.lp:    mov ch, [esi]       ; mov next char to ch
        cmp ch, '0'         ; check if less then char '0'
        jl .err             ; if so, raise error
        cmp ch, '9'         ; check if higher than char '9'
        jg .err             ; if so, raise error
        sub ch, '0'         ; char2num in edx
        mul ebx             ; mult eax by 10 (so that carry flag is set)
        jc .err 
        xor edx, edx        ; zero out digit place
        mov dl, ch          ; put ch in dl
        add eax, edx        ; add the digit
        jc .err 
        dec cl              ; decrease length
        cmp cl, 0           ; check if out of digits
        jz .quit            ; if so, quit subpr
        inc esi             ; increment esi to next char adr
        jmp .lp             ; else, repeat loop
.err:   mov cl, 1           ; if it was an .error, put 1 in cl
.quit:  pop ebx             ; restore ebx
;add cl, '0'
;PUTCHAR cl
;sub cl, '0'
        pop esi             ; restore esi
        mov esp, ebp        ; deinit stack frame
        pop ebp
        ret
