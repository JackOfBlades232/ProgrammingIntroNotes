;; 4_34/start.asm ;;
global _start
extern main
section         .text
_start:         mov     ecx, [esp]
                mov     eax, esp
                add     eax, 4   
                push    eax
                push    ecx
                call    main
                add     esp, 8  
                mov     ebx, eax
                mov     eax, 1
                int     80h
