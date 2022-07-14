%include "../../stud_io.inc"
global _start

section .data
fibon   dw 1, 1, 2, 3, 5, 8, 13, 21 ; 8 2-byte words to array
quad    dd 2af3h, $2af3, 0x2af3     ; 3 ways to write quad nums
oct     dw 754q         ; oct number
bin     db 1101b        ; bin number
fig7    db '7'          ; char
welmsg  db 'Welcome!'   ; string/array of chars
panic   db 'So I say: "Don', "'", 't panic"'    ; dealing with parentheses


section .bss
string  resb 20         ; 20 bytes to string
count   resw 256        ; 256 2-byte words to count
x       resd 1          ; 1 4-byte word to x
matrix  resd 10*15      ; memory for matrix

section .text
fcircle dw 360          ; initialize const
_start: mov eax, ebx    ; cp from ebx to eax
        mov ecx, 5      ; put 5 in ecx
        mov [x], ecx    ; cp from ecx to mem in x
        mov edx, [x]    ; cp from mem in x to edx, cant move from mem to mem
        mov edx, x      ; cp adress x to edx
        mov ebx, [eax]  ; cp value of mem in eax to ebx
        mov eax, [count+ebx+2*edi]  ; complex adress for array index search
        mov edx, [matrix+eax+4*ebx] ; search matrix
        lea eax, [1000+ebx+8*ecx]   ; put adress in eax
        mov dword [x], 25   ; put 25 in mem in x, have to specify type
        add eax, ebx    ; add ebx to eax and put in eax
        sub [x], ecx    ; subtract ecx from mem in x and put in mem in x
        add edx, 12     ; add 12 to edx and put in edx
        add dword [x], 12   ; add 12 to mem at x, have to spec type
        add eax, ecx    ; add lower parts
        adc edx, ebx    ; add upper parts with overflow
        sub eax, ecx    ; sub lower parts
        sbb edx, ebx    ; sub upper parts with overflow
        cmp ecx, 25     ; compare and update flag
        mul bl          ; mult al by bl, res in ax
        mul bx          ; mult ax by bx, res in dx:ax
        mul ebx         ; mult eax by ebx, res in edx:eax
        imul bx         ; same, but with sign
        div bl          ; div ax by bl, q in al, r in ah
        div bx          ; div dx:ax by bx, q in ax, r in dx
        div ebx         ; same, with e-s
        idiv bx         ; signed
        cbw             ; convert al to ax (fill ah with sign of al)
        cwd             ; convert ax to dx:ax
        cwde            ; convert ax to eax
        cdq             ; convert eax to edx:eax
        FINISH
