;; 3_37/write_oct.asm ;;
%include "../useful_macros.inc"
global write_oct

section .text
; proc write oct : write oct number in digits to memory (32 bit number)
; adr == [ebp+8], number == [ebp+12] : digits->adr
write_oct:
        push ebp
        mov ebp, esp
        cld                     ; prep flag for edi
        push edi                ; edi will be corrupted when writing digits
        mov edi, [ebp+8]
        mov eax, [ebp+12]
        mov ecx, 8              ; divisor to ecx
        xor edx, edx            ; prep edx for division
        div ecx                 ; divide eax by 8, remainder to edx
        add edx, '0'            ; convert to digit
        push edx                ; and store in stack (save first digit always)
.lp:    cmp eax, 0
        jz .store
        xor edx, edx            ; prepare eax:edx pair for division
        div ecx
        add edx, '0'
        push edx
        jmp .lp
.store: mov ecx, ebp            ; calc number of digits with esp and ebp
        sub ecx, esp            ; sub one dword for stored edi, 
        sub ecx, 4              ;   div by bytes in dword 
        shr ecx, 2              
.st_lp: pop eax                 ; now, extract digits from stack and store
        stosb                   ; save al to mem and advance edi
        dec ecx
        test ecx, ecx
        jnz .st_lp
        pop edi
        mov esp, ebp
        pop ebp
        ret
