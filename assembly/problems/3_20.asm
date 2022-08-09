%include "../stud_io.inc"
global _start

section .bss
cflg    resb 1
sum     resd 1
sbtr    resd 1
mlt     resd 2
digits  resb 10

section .text
_start: xor ch, ch
lp:     xor cl, cl
        xor ebx, ebx
lp1:    GETCHAR    
        cmp al, '0'
        jl check
        cmp al, '9'
        jg check
        or cl, 1
        sub al, '0'
        shl ebx, 1 
        lea ebx, [ebx*5]   
        add ebx, eax   
        jmp lp1        
check:  cmp cl, 1
        jnz err
        cmp ch, 0
        jnz chnl
        cmp al, ' '
        jmp dcerr
chnl:   cmp al, 10
dcerr:  jnz err
        inc ch
        cmp ch, 1
        jnz snc
        mov [sum], ebx
        mov [sbtr], ebx
        jmp lp
snc:    mov eax, [sum]
        xor edx, edx
        mul ebx
        mov [mlt], eax
        mov [mlt+4], edx
        add [sum], ebx
        sub [sbtr], ebx
        mov byte [cflg], 0
chspr:  cmp byte [cflg], 1
        jz sbpr
chmpr:  cmp byte [cflg], 2
        jz mpr
        mov eax, [sum]
        jmp print
sbpr:   mov eax, [sbtr]
        jmp print
mpr:    mov eax, [mlt]
print:  xor edx, edx
        mov ebx, 10
        mov ecx, 10
lp3:    div ebx
        mov [digits+ecx-1], dl
        xor dl, dl
        loop lp3
        mov esi, digits
        cld
        mov ecx, 10
        xor bl, bl
lp4:    lodsb
        cmp bl, 0
        jnz prdgt
        cmp al, 0
        jnz prdgt
        loop lp4
prdgt:  or bl, 1
        add al, '0'
        PUTCHAR al
        loop lp4
        PUTCHAR ' '
        inc byte [cflg]
        cmp byte [cflg], 3
        jnz chspr
        PUTCHAR 10
        FINISH
err:    cmp eax, -1
        jz prerr
        cmp eax, 10
        jz prerr
        GETCHAR 
        jmp err
prerr:  PRINT "Incorrect input"
        PUTCHAR 10
        FINISH 1

