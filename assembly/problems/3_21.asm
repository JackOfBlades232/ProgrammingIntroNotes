; Here, finally, the numbers are signed, overflows still ignored

%include "../stud_io.inc"
global _start

section .bss
digits  resb 10         ; arr for 10 digits (for printing number)
num1    resd 1          ; first number slot (4 bytes)
sign    resb 1          ; 1 byte to remember the operation

section .text
_start: xor ch, ch      ; zero out input number counter
rdlp:   xor ebx, ebx    ; zero out number accumulator
        xor cl, cl      ; zero out first digit flag
        xor dl, dl      ; zero out number sign flag
lp1:    GETCHAR         ; read next char/digit
        cmp eax, -1     ; check if eof
        jz quit         ; if so, just quit
        cmp eax, 10     ; else, check if eoln
        jz check        ; if so, go to check correctness of input
        cmp eax, '-'    ; else, check if its a minus
        jnz mcmp        ; if not, go to check if digit
        cmp cl, 0       ; else, check if there were no digits before
        jnz err         ; if the were, go to error
        or dl, 1        ; else, set the sign flag
        jmp lp1         ; and go back to input loop
mcmp:   cmp eax, '0'    ; now, check if char is digit 1
        jl err          ; if not, error
        cmp eax, '9'    ; and digit check 2
        jg err          ; if failed, error
        or cl, 1        ; set first digit flag
        shl ebx, 1      ; multiply accumulated number by 2
        lea ebx, [5*ebx]    ; and by 5
        sub eax, '0'    ; convert new digit to num
        add ebx, eax    ; and add to accumulator
        jmp lp1         ; and repeat the input loop
check:  cmp cl, 0       ; if line was read, check if there were any digits
        jz err          ; if not, error
        cmp dl, 0       ; now, check if the number was negative
        jz dcn          ; if not, skip next step
        neg ebx         ; if yes, take the negative if the accumulator
dcn:    cmp ch, 0       ; now, check if this was the first of 2 numbers
        jnz mv2         ; if it wasnt, then go to handle calculations
        mov [num1], ebx ; else, store it in mem
        inc ch          ; and increment the digit counter
        GETCHAR         ; read the op sign from next line
        cmp eax, -1     ; check if eof
        jz quit         ; in that case, quit
        cmp eax, '*'    ; check sign correctness
        jz mvsgn        ; if correct, go to store sign
        cmp eax, '+'    ; =|=
        jz mvsgn        ; =|=
        cmp eax, '-'    ; =|=
        jz mvsgn        ; =|=
        cmp eax, '/'    ; =|=
        jz mvsgn        ; =|=
        jmp err         ; if not correct, error
mvsgn:  mov [sign], al  ; now store the opsign in mem
        GETCHAR         ; read the next char
        cmp eax, -1     ; check if eof
        jz quit         ; in that case, quit
        cmp eax, 10     ; check if eoln
        jnz err         ; if not, error
        jmp rdlp        ; else, go to read another number
mv2:    mov eax, [num1] ; move the first number to eax (second in ebx already)
        cdq             ; extend the signed number to eax:edx
        cmp byte [sign], '*'    ; check op sign
        jz mlt          ; if this one, go to respective calculation
        cmp byte [sign], '+'    ; =|=
        jz sum          ; =|=
        cmp byte [sign], '-'    ; =|=
        jz sbtr         ; =|=
        cmp byte [sign], '/'    ; =|=
        jz dvd          ; =|=
mlt:    imul ebx        ; case mult, multiply eax by ebx
        jmp print       ; and go to print
sum:    add eax, ebx    ; same for sum
        jmp print       ; =|= 
sbtr:   sub eax, ebx    ; and for diff  
        jmp print       ; =|=
dvd:    cmp ebx, 0      ; for division, check if divisor is 0
        jz err          ; if so, go to error
        idiv ebx        ; if all ok, perform division
print:  test eax, eax   ; in print, check eax sign
        jns prnum       ; if pos, go to print the meat
        PUTCHAR '-'     ; else, first output a minus
        neg eax         ; and invert eax
prnum:  xor edx, edx    ; now, the same old story as in 3_19 (output number)
        mov ebx, 10     ; =|=    
        mov ecx, 10     ; =|=
lp2:    div ebx         ; =|=
        mov [digits+ecx-1], dl  ; =|=
        xor dl, dl      ; =|=
        loop lp2        ; =|=
        mov esi, digits ; =|=
        cld             ; =|=
        mov ecx, 10     ; =|=
        xor bl, bl      ; =|=
lp3:    lodsb           ; =|=
        cmp bl, 0       ; =|=
        jnz prdgt       ; =|=
        cmp al, 0       ; =|=
        jnz prdgt       ; =|=
        cmp cl, 1       ; =|=
        jz prdgt        ; =|=
        loop lp3        ; =|=
prdgt:  or bl, 1        ; =|=
        add al, '0'     ; =|=
        PUTCHAR al      ; =|=
        loop lp3        ; =|=
        jmp quit        ; and go to quit
err:    cmp eax, 10     ; if encountered error, check for eoln
        jz prerr        ; if eoln, go to print the error
        GETCHAR         ; else, clean buffer
        jmp err         ; repeat until buffer empty
prerr:  PRINT "Error"   ; print the message
        PUTCHAR 10      ; new line after it
        jmp _start      ; and restart the program
quit:   PUTCHAR 10      ; if successful finish, aslo newline
        FINISH          ; and exit with macro
