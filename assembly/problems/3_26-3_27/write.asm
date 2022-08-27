global write_num

section .text
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
