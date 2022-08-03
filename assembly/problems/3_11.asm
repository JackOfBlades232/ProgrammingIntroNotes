%include "../stud_io.inc"
global _start

section .text
_start: xor ecx, ecx    ; reset counter
cycle:  GETCHAR         ; read next char
        cmp eax, 10     ; check eoln
        jz stars        ; if eoln break
        cmp eax, '0'    ; check if lower than '0'
        jl cycle        ; if lower continue
        cmp eax, '9'    ; check if higher than '9'
        jg cycle        ; if higher continue
        sub eax, '0'    ; char2num
        add ecx, eax    ; add to counter
        jmp cycle       ; repeat
stars:  cmp ecx, 0      ; check if counter is zero
        jz quit         ; if zero quit
        PUTCHAR '*'     ; print next star
        loop stars      ; loop while ecx != 0
        PUTCHAR 10      ; new line after output
quit:   FINISH          ; exit
