%include "../../stud_io.inc"
global input_str

section .text
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
        jmp .quit           ; and jump to quit
.mem_quit:
        GETCHAR             ; if mem quit, eat up next char after out of mem
        cmp al, '0'         ; check if less then char '0'
        jl .good_mem        ; if so, all ok
        cmp al, '9'         ; check if higher than char '9'
        jg .good_mem        ; if so, all ok
        mov dl, -2          ; else, put overflow sign in dl
.good_mem:
        mov dl, al          ; if used all mem, put next char in dl
.quit:  mov eax, [ebp+12]   ; put adr in eax
        pop edi             ; restore edi
        mov esp, ebp        ; deinit stack frame
        pop ebp
        ret
