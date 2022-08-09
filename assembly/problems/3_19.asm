%include "../stud_io.inc"
global _start

section .bss
digits  resb 10

section .text
_start: xor ebx, ebx
lp1:    GETCHAR
        cmp eax, -1
        jz dgs
        cmp eax, 10
        jz lp1
        inc ebx
        jmp lp1
dgs:    mov eax, ebx
        xor edx, edx
        mov ebx, 10
        mov ecx, 10
lp2:    div ebx
        mov [digits+ecx-1], dl
        xor dl, dl
        loop lp2
        mov esi, digits
        cld
        mov ecx, 10
        xor bl, bl
lp3:    lodsb
        cmp bl, 0
        jnz prdgt
        cmp al, 0
        jnz prdgt
        loop lp3
prdgt:  or bl, 1
        add al, '0'
        PUTCHAR al
        loop lp3
        PUTCHAR 10
quit:   FINISH
