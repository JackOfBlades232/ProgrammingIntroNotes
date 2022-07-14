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
fcircle dw 360          ; initialize immutable val
_start: mov eax, ebx    ; cp from ebx to eax
        mov ecx, 5      ; put 5 in ecx
        mov [x], ecx    ; cp from ecx to mem in x
        mov edx, [x]    ; cp from mem in x to edx, cant move from mem to mem
        mov edx, x      ; cp adress x to edx
        mov ebx, [edx]  ; cp value of mem in eax to ebx
        mov eax, [count+ebx+2*edi]  ; complex adress for array index search
        mov edx, [matrix+eax+4*ebx] ; search matrix
        lea eax, [1000+ebx+8*ecx]   ; put adress in eax
        mov dword [x], 25   ; put 25 in mem in x, have to specify type
        mov eax, 5      ; put 5 in eax
        mov edx, 6      ; put 6 in edx
        add eax, ebx    ; add ebx to eax and put in eax
        sub [x], ecx    ; subtract ecx from mem in x and put in mem in x
        add edx, 12     ; add 12 to edx and put in edx
        add dword [x], 12   ; add 12 to mem at x, have to spec type
        add eax, ecx    ; add lower parts
        adc edx, ebx    ; add upper parts with overflow
        sub eax, ecx    ; sub lower parts
        sbb edx, ebx    ; sub upper parts with overflow
        cmp ecx, 25     ; compare and update flags
        mov eax, 320    ; put 320 in eax
        mov ebx, 442    ; put 442 in ebx
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
        cwde            ; ax to eax
        cdq             ; eax to edx:eax
        add eax, ecx    ; add ecx to eax
        jz sjump        ; jump if ZF=1 (same with js, jc, jo, jp) def short
        jnz near next   ; jump if ZF=0 (same with jns, et cetera)
        ; js [eax]      ; cant do this, only labels and const for cond jump 
        jmp sjump       ; uncond jump to exit
sjump:  jmp short next  ; optimized jump if adr is within 127 bytes
        mov eax, 4      ; put 4 in eax
        mov ebx, 3      ; put 3 in ebx
        cmp eax, ebx    ; compare eax, ebx
next:   jle sjump       ; jump if eax is less or equal to ebx
        ; there are jumps for all comp results, different for signed and uns
        mov ecx, 0      ; put 0 in ecx
cycle:  cmp ecx, 5      ; compare counter to 5
        jnl cycle_quit  ; break if counter not less then 5
        inc ecx         ; increment counter
        jmp cycle       ; repeat
cycle_quit:
        cmp ecx, 5      ; compare ecx to 5
        jnz else_branch ; if not equal, go to else
        mov ecx, 0      ; if body
        jmp if_quit     ; skip else branch
else_branch:
        mov ecx, 1      ; else body
if_quit:
        xor ecx, ecx    ; better way to zero out
        FINISH          ; exit macro
