%include "../stud_io.inc"
global _start

section .text
_start: xor ebx, ebx    ; zero out ebx (+ counter)
        xor edx, edx    ; zero out edx (- counter)
lp1:    GETCHAR         ; read char from input
        cmp eax, 10     ; check if eoln
        jz mult         ; if eoln, jump out
        cmp eax, '+'    ; check if +
        jnz minus       ; if not, go to - check
        inc ebx         ; if yes, increment counter
minus:  cmp eax, '-'    ; check if -
        jnz lp1         ; if not, go to read char
        inc edx         ; if yes, increment counter
        jmp lp1         ; end of cycle
mult:   mov eax, edx    ; prepare for multiplication
        mul ebx         ; multiply counters and store in edx:eax
        cmp edx, 0      ; check if there is anything in edx
        jz print2       ; if not, go to printing * from eax
        mov ebx, 11111111h  ; put all 1s in ebx
lp2:    or ecx, ebx     ; make ecx all ones (print max val in 32bit reg)
nestlp: PUTCHAR '*'     ; print the star 
        loop nestlp     ; loop
        PUTCHAR '*'     ; print the 2^32-th star
        dec edx         ; decrement edx
        jnz lp2         ; if anything left in edx, repeat
print2: cmp eax, 0      ; check if eax is empty
        jz quit         ; if empty, exit
        mov ecx, eax    ; move eax into counter
lp3:    PUTCHAR '*'     ; print the star
        loop lp3        ; repeat
quit:   PUTCHAR 10      ; put new line after output (for ease eve after empty)
        FINISH          ; exit with macro
