%include "../../stud_io.inc"
global _start

section .bss
string  resb 27         ; string to be reversed

section .text
_start: mov ecx, 26         ; string non-zero char counter
lp0:    mov bl, 'a'         ; set ebx to first char
        add bl, cl          ; offset by ecx
        dec bl              ; shift by 1
        mov [string+ecx-1], bl  ; put in string
        loop lp0            ; repeat, fill str with alphabet
        mov byte [string+26], 0 ; put 0 in str end
        mov esi, string     ; put string adr in esi
        xor bl, bl          ; zero out ebx, ecx already 0
lp1:    mov bl, [esi+ecx]   ; read next byte from string
        cmp bl, 0           ; check eol
        jz lpquit           ; if so, end cycle
        push ebx            ; push byte as dword to stack
        inc ecx             ; inc counter
        jmp lp1             ; repeat
lpquit: jecxz done          ; if string is empty, end all
        mov edi, esi        ; put string adr in esi
lp2:    pop ebx             ; pop char from stack
        mov [edi], bl       ; put the char in string
        PUTCHAR bl          ; print the char
        inc edi             ; next adr
        loop lp2            ; repeat ecx times
        PUTCHAR 10          ; new line if anything was output
done:   FINISH              ; exit with macro
