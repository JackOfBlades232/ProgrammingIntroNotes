%include "../stud_io.inc"
global _start

section .bss
digits  resb 10             ; array of digits for i-o

section .text
; read unsigned number (address=eax, length=cl) : number->eax, success flag->cl
; overflow raises .error
read_num:                   ; SUBPR START
        push esi            ; save prev esi value, will be using it
        push ebx            ; also save ebx
        cmp cl, 0           ; check if length is 0
        jz .err             ; if so, go to err
        mov esi, eax        ; put adr in esi
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
        pop esi             ; restore esi
        ret
; write unsigned num (number=eax, address=ecx) : string->address, length->cl
write_num:                  ; SUBPR START
        push edi            ; save prev edi value, will be using it
        push ebx            ; same with ebx
        mov edi, ecx        ; mov address to edi
        mov ebx, 10         ; put divisor in ebx
        xor ecx, ecx        ; zero out digit counter
.stack_lp:
        xor edx, edx        ; prepare edx for division
        div ebx             ; divide eax:edx by ecx
        add edx, '0'        ; convert digit to char
        push edx            ; push digit to stack
        inc ecx             ; increment digit counter
        cmp eax, 0          ; check if anything left
        jnz .stack_lp       ; if anything left, repeat loop
        mov dl, cl          ; save cl in dl
.write_lp:
        pop eax             ; pop digit to eax
        stosb               ; move digit to [edi] and inc edi
        loop .write_lp      ; repeat for num of digits iterations
        mov cl, dl          ; restore cl
        pop ebx             ; restore ebx
        pop edi             ; restore edi
        ret
; output char string (address=eax, length=cl)
output_str:                 ; SUBPR START
        push esi            ; save prev esi value, will be using it
        cmp cl, 0           ; check if string is empty
        jz .quit            ; if so, exit subpr
        mov esi, eax        ; prepare esi for output
.out_lp: 
        lodsb               ; read char to eax
        PUTCHAR al          ; output the digit
        dec cl              ; decrease counter
        cmp cl, 0           ; check if counter is 0
        jnz .out_lp         ; if not, repeat
.quit:  pop esi             ; restore esi
        ret
; read string from input (address=eax, length=cl) :
; num address->eax, symbols read->cl, exit symbol->dl
input_str:
        push edi            ; save prev edi value, will be using it
        push eax            ; save string address
        cmp cl, 0           ; check if free mem is 0
        jz .mem_quit        ; if so, exit subpr by mem
        mov edi, eax        ; move address to edi
        mov dl, cl          ; store init cl val in dl
.in_lp: GETCHAR             ; get next char from input to eax
        cmp al, '0'         ; check if less then char '0'
        jl .in_lp_fin       ; if so, quit loop
        cmp al, '9'         ; check if higher than char '9'
        jg .in_lp_fin       ; if so, quit loop
        stosb               ; put digit in [edi] and inc edi
        loop .in_lp         ; repeat until out of mem
.in_lp_fin:
        sub dl, cl          ; find out number of read digits
        xchg dl, cl         ; swap dl and cl values
        cmp dl, 0           ; check if ran out of mem
        jz .mem_quit        ; if so, to mem quit 
        mov dl, al          ; put culprit char (or eof code) in dl
        cmp dl, 10          ; check if breaking char was eoln
        jnz .buf_clear_lp   ; if it was not, proceed to clear buffer
        jmp .quit           ; and jump to quit
.mem_quit:
        mov dl, -2          ; put sign of running out of mem in dl
.buf_clear_lp:              ; have to clear buffer until eol
        GETCHAR             ; get next char from input
        cmp eax, 10         ; check if eoln
        jnz .buf_clear_lp   ; if not, go to eat next char
.quit:  pop eax             ; restore address
        pop edi             ; restore edi
        ret
; main code
_start: xor bl, bl          ; prepare read counter
input:  mov eax, digits     ; init param adr
        mov cl, 10          ; init param len
        call input_str      ; call subpr 
        call read_num       ; call subpr (all params in place)
        cmp cl, 0           ; check if exit code was 0
        jnz of              ; if it wasnt, quit with overflow warning
        push eax            ; push first number to stack
        inc bl              ; increment read counter
        cmp bl, 2           ; check if read 2 numbers
        jl input            ; if not, read another number
        xor ebp, ebp        ; use ebp as output num counter
        pop ebx             ; pop second number to ebx
c_lp:   mov eax, [esp]      ; put first number in eax
        cmp ebp, 0          ; check if first iter
        jz c_sum            ; if so, go to calc sum
        cmp ebp, 1          ; check if second iter
        jz c_diff           ; if so, go to calc diff
        mul ebx             ; else, calc prod
        jmp of_chk          ; and go to output
c_diff: sub eax, ebx        ; if diff, calc diff
        jmp of_chk          ; and jump to output
c_sum:  add eax, ebx        ; else, add ebx to eax
of_chk: jc of               ; if carry flag, go to overflow
        mov ecx, digits     ; init adr param (number already in eax)
        call write_num      ; call subpr
        mov eax, digits     ; init adr param (for output, length in cl already)
        call output_str     ; call subpr
        PUTCHAR ' '         ; put space after
c_lp_fin:
        inc ebp             ; increment iter counter
        cmp ebp, 3          ; check if all iterations done
        jl c_lp             ; if not, go to calc loop start
        PUTCHAR 10          ; newline after output
        jmp quit            ; and quit
of:     PRINT "OVERFLOW"    ; warn about overflow
        PUTCHAR 10          ; and new line
quit:   FINISH              ; exit with macro
