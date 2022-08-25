%include "../../stud_io.inc"
global output_str

section .text
; output char string (address=[ebp+12], length=[ebp+8])
output_str:                 ; SUBPR START
        push ebp            ; init stack frame
        mov ebp, esp
        push esi            ; save prev esi value, will be using it
        mov ecx, [ebp+8]    ; put len param in ecx
        mov esi, [ebp+12]   ; put adr param in esi
        cmp cl, 0           ; check if string is empty
        jz .quit            ; if so, exit subpr
.out_lp: 
        lodsb               ; read char to eax
        PUTCHAR al          ; output the digit
        dec cl              ; decrease counter
        cmp cl, 0           ; check if counter is 0
        jnz .out_lp         ; if not, repeat
        PUTCHAR 10          ; new line after
.quit:  pop esi             ; restore esi
        mov esp, ebp        ; deinit stack frame
        pop ebp
        ret
