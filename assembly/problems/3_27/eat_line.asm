%include "../../stud_io.inc"
global eat_input

section .text
; eat up input from line ( last char=[ebp+8] )
eat_input:
        push ebp            ; init stack frame
        mov ebp, esp
        mov eax, [ebp+8]    ; put last char in eax
.lp:    cmp al, -1          ; check if eof
        jz .quit            ; if so, quit
        cmp al, 10          ; check if eoln
        jz .quit            ; if so, quit
        GETCHAR             ; else, eat another char
        jmp .lp             ; and repeat
.quit:  mov esp, ebp        ; deinit stack frame
        pop ebp
        ret
