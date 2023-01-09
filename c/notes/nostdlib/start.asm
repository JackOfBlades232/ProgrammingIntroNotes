;; nostdlib/start.asm ;;
global _start
extern main
section         .text
_start:         mov     ecx, [esp]  ; argc to ecx
                mov     eax, esp
                add     eax, 4      ; argv to eax
                push    eax
                push    ecx
                call    main
                add     esp, 8      ; clean the stack
                mov     ebx, eax    ; call _exit with main return val as code
                mov     eax, 1
                int     80h
