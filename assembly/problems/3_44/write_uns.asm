;; 3_44/write_uns.asm ;;
global write_uns

section .text
; write unsigned num (number=[ebp+8], address=[ebp+12], max_len=[ebp+16]) :
;   string->address, length->ecx
write_uns:                  ; SUBPR START
        push ebp            ; init stack frame
        mov ebp, esp
        push edi            
        push ebx           
        mov eax, [ebp+8]   ; mov number to eax 
        mov edi, [ebp+12]    ; mov address to edi
        mov ebx, 10         ; put divisor in ebx
        xor ecx, ecx        
.stack_lp:
        xor edx, edx       
        div ebx           
        add edx, '0'     
        push edx        
        inc ecx        
        cmp ecx, [ebp+16]   ; check if exhausted length
        jae .save_ecx
        cmp eax, 0    
        jnz .stack_lp
.save_ecx:
        mov edx, ecx        ; save ecx
.write_lp:
        pop eax        
        stosb         
        loop .write_lp 
        mov ecx, edx        ; save edx
        pop ebx             ; restore ebx
        pop edi             ; restore edi
        mov esp, ebp        ; deinit stack frame
        pop ebp
        ret
