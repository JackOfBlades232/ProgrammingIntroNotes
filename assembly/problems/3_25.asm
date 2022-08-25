%include "../stud_io.inc"
global _start

section .bss
in_str  resb 10             ; 10 bytes for reading input
out_str resb 10             ; 10 bytes for output of functions

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
        pop esi             ; restore esi
        mov esp, ebp        ; deinit stack frame
        pop ebp
        ret
; write unsigned num (number=[ebp+12], address=[ebp+8]) :
;   string->address, length->cl
write_num:                  ; SUBPR START
        push ebp            ; init stack frame
        mov ebp, esp
        push edi            ; save prev edi value, will be using it
        push ebx            ; same with ebx
        mov edi, [ebp+8]    ; mov address to edi
        mov eax, [ebp+12]   ; mov number to eax 
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
        mov esp, ebp        ; deinit stack frame
        pop ebp
        ret
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
; read string from input (address=[ebp+12], length=[ebp+8]) :
;   num address->eax, symbols read->cl, exit symbol->dl
input_str:
        push ebp            ; init stack frame
        mov ebp, esp
        push edi            ; save prev edi value, will be using it
        mov ecx, [ebp+8]    ; put length param in ecx
        mov edi, [ebp+12]   ; put adr param in esi
        cmp cl, 0           ; check if free mem is 0
        jz .mem_quit        ; if so, exit subpr by mem
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
.quit:  mov eax, [ebp+12]   ; put adr in eax
        pop edi             ; restore edi
        mov esp, ebp        ; deinit stack frame
        pop ebp
        ret
; main code
_start: push dword in_str   ; push param adr
        push dword 10       ; push param len
        call input_str      ; call subpr 
        add esp, 8          ; clear stack from 2 params
        push eax            ; push adr param to stack
        push ecx            ; push length param to stack
        call read_num       ; call subpr (all params in place)
        add esp, 8          ; clear stack from 2 params
        cmp cl, 0           ; check if exit code was 0
        jnz quit            ; if it wasnt, quit program
        push eax            ; push number param
        push dword out_str  ; push adr param 
        call write_num      ; call subpr
        add esp, 8          ; clear stack from 2 params
        push dword out_str  ; push param adr
        push ecx            ; push param len
        call output_str     ; call subpr
        add esp, 8          ; clear stack from 2 params
quit:   FINISH              ; exit with macro
