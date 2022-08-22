%include "../../stud_io.inc"
global _start

section .data
ex_ptr1 db "a?r*adabra", 0      ; pattern for testing
ex_ptr2 db "abracadabra", 0     ; no-spec symbol pattern for testing
ex_str1 db "abracadabra", 0     ; matching string
ex_str2 db "acraadabra", 0      ; another matching string

section .text
; match (pattern=[ebp+8], string=[ebp+12])
match:                      ; SUBPR START
        push ebp            ; init stack frame
        mov ebp, esp
        sub esp, 4          ; local var I will be at ebp-4
        push esi            ; save prev esi and edi val
        push edi            ;   (by conv not save only eax, edx, ecx)
        mov esi, [ebp+8]    ; string param to esi
        mov edi, [ebp+12]   ; pattern param to edi
.again:                     ; come back here after matching next symbol
        cmp byte [edi], 0   ; check if pattern ended
        jnz .not_end        ; if not, jump
        cmp byte [esi], 0   ; check if string ended
        jnz near .false     ; if not, return false
        jmp .true           ; else, true
.not_end:                   ; if pattern not over
        cmp byte [edi], '*' ; is next pattern char a * ?
        jnz .not_star       ; if not, jump out of here
        mov dword [ebp-4], 0    ; I := 0
.star_loop:
                            ; prepare for recursive call
        mov eax, edi        ; first second arg
        inc eax             ; set pattern to next symbol
        push eax
        mov eax, esi        ; now first arg
        add eax, [ebp-4]    ; take the string from I-th elem
        push eax
        call match          ; call self with new params
        add esp, 8          ; clear stack after call
        test eax, eax       ; check, what was returned
        jnz .true           ; if true, go to true
        add eax, [ebp-4]    ; if false (eax=1), then try taking I+1 symbol
        cmp byte [esi+eax], 0   ; check if this runs us out of string
        jz .false           ; if it does, return false
        inc dword [ebp-4]   ; else, try I++
        jmp .star_loop      ; to start of I cycle
.not_star:                  ; now, if pattern starts not with star
        mov al, [edi]       
        cmp al, '?'         ; is it '?'
        jz .quest           ; if so, jump out of here
        cmp al, [esi]       ; if not, cmp it to first char of string
        jnz .false          ; if dont match, return false
        jmp .goon           ; if match, continue looking
.quest:                     ; case '?'
        cmp byte [esi], 0   ; the string has to be not empty
        jz .false           ; if empty, ret false
.goon:  inc esi             ; symbols matched, move pointers
        inc edi
        jmp .again          ; go all the way back to the cycle
.true:                      ; returning true
        mov eax, 1
        jmp .quit
.false:                     ; returning false
        xor eax, eax        ; 0 in eax
.quit:                      ; wrap up
        pop edi             ; restore edi and esi
        pop esi
        mov esp, ebp        ; common deinit
        pop ebp
        ret                 ; end
; main code
_start: push dword ex_ptr2  ; push pattern params
        push dword ex_str2  ; push string param
        call match          ; call the subprogram
        add esp, 8          ; clear the stack from params
        add al, '0'         ; output to digit
        PUTCHAR al          ; print it
        PUTCHAR 10          ; newline after it
        FINISH              ; quit with macro
