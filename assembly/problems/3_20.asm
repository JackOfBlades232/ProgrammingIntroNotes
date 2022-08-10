; Here i treat numbers as unsigned, ignoring possible overflow

%include "../stud_io.inc"
global _start

section .bss
cflg    resb 1          ; one byte for output counter
sum     resd 1          ; 4 bytes for sum
diff    resd 1          ; 4 bytes for diff
prod    resd 1          ; 4 bytes for product
digits  resb 10         ; 10 bytes for digits

section .text
_start: xor ch, ch      ; clear input word counter
lp:     xor cl, cl      ; clear first digit flag
        xor ebx, ebx    ; clear number accumulator
lp1:    GETCHAR         ; read next char from input
        cmp al, '0'     ; check if below '0'
        jl check        ; if so, go to check for incorrect input
        cmp al, '9'     ; else, check if above '9'
        jg check        ; again, if so, go to check
        or cl, 1        ; if digit, set the first digit flag
        sub al, '0'     ; convert char to num
        shl ebx, 1      ; multiply accumulated number by 2
        lea ebx, [ebx*5]    ; and then by 5 
        add ebx, eax    ; and add the digit to it
        jmp lp1         ; repeat the digit-read loop
check:  cmp cl, 1       ; check if there was a single digit read
        jnz err         ; if not, go to error handling
        cmp ch, 0       ; check if this was the first word of input
        jnz chnl        ; if not, go to check if the last char read is #10
        cmp al, ' '     ; if it was the first one, check if we read a space
        jmp dcerr       ; skip over new line check
chnl:   cmp al, 10      ; if this was word 2, check if newline
dcerr:  jnz err         ; if not ' ' or not #10, go to error
        inc ch          ; increment input word counter
        cmp ch, 1       ; check if 1 word was inputted
        jnz snc         ; if not, go to calculations
        mov [sum], ebx  ; else, put this word in sum 
        mov [diff], ebx ; and diff (memory)
        jmp lp          ; and repeat for second word
snc:    mov eax, [sum]  ; after words are read, mov the first one to eax
        xor edx, edx    ; clear edx for multiplication
        mul ebx         ; multiply eax by ebx (second number)
        mov [prod], eax ; move the product to memory
        add [sum], ebx  ; add second number to the sum
        sub [diff], ebx ; and subtract it from the diff
        mov byte [cflg], 0  ; now, set output word counter to 0
chspr:  cmp byte [cflg], 1  ; check if it is one (after jumps)
        jz sbpr         ; if so, go to print diff
chmpr:  cmp byte [cflg], 2  ; else, check if it is two
        jz mpr          ; in that case, go to print prod
        mov eax, [sum]  ; move the sum to eax (case 1)
        jmp print       ; and go to print the number
sbpr:   mov eax, [diff] ; case 2: diff
        jmp print       ; and print
mpr:    mov eax, [prod] ; case 3: prod (last one)
print:  xor edx, edx    ; clear edx for division
        mov ebx, 10     ; put divisor in ebx
        mov ecx, 10     ; max digit counter to ecx
lp3:    div ebx         ; divide eax by 10
        mov [digits+ecx-1], dl  ; put the remainder to the end of digits arr
        xor dl, dl      ; zero out edx for div again
        loop lp3        ; repeat until ecx=0
        mov esi, digits ; put the pointer to first digit in esi
        cld             ; set normal direciton
        mov ecx, 10     ; again, set counter to max digit number
        xor bl, bl      ; clear flag for first non-zero digit
lp4:    lodsb           ; read from arr to al and inc esi
        cmp bl, 0       ; check if flag is 0
        jnz prdgt       ; if not, go to print digit
        cmp al, 0       ; if it is, check if digit is non-zero
        jnz prdgt       ; if it is, go to print it
        cmp cl, 1       ; if just zeroes, check if last digit
        jz prdgt        ; if it is, print the one zero
        loop lp4        ; if not, just continue with the loop
prdgt:  or bl, 1        ; int printing, set the non-zero flag
        add al, '0'     ; convert num to char
        PUTCHAR al      ; output the char
        loop lp4        ; repeat the output cycle
        PUTCHAR ' '     ; print a space
        inc byte [cflg] ; increment the output word counter
        cmp byte [cflg], 3  ; check if printed 3 words
        jnz chspr       ; if not, go back to calculating digits
        PUTCHAR 10      ; if we are done, newline
        FINISH          ; and quit with macro
err:    cmp eax, -1     ; if error, check if eof
        jz prerr        ; if so, go to print error
        cmp eax, 10     ; check if eoln
        jz prerr        ; if eoln, again go to print error
        GETCHAR         ; else, eat up the buffer
        jmp err         ; and repeat, until buffer is empty
prerr:  PRINT "Incorrect input" ; output the error text
        PUTCHAR 10      ; put new line after it
        FINISH 1        ; and exit with code 1

