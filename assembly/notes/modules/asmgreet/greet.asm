;; asmgreet/greet.asm ;;
global _start                       ; this is the main module, using others
extern putstr                       ; extern = require an outside label when
extern getstr                       ;   linking
extern quit

section .data                       ; here, describing messages texts (end w 0)
nmq     db 'Hello, what is your name?', 10, 0
pmy     db 'Pleased to meet you, dear ', 0
exc     db '!', 10, 0

section .bss                        ; memory for input buffer
buf     resb 512
buflen  equ $-buf

section .text                       ; main code
; jist: ask for name, when answered, greet by name
_start: push dword nmq              ; output question
        call putstr
        add esp, 4
        push dword buflen           ; read name from stdin
        push dword buf
        call getstr
        add esp, 8
        push dword pmy              ; output first part of greeting
        call putstr
        add esp, 4
        push dword buf              ; output name
        call putstr
        add esp, 4
        push dword exc              ; ouptut ! and new line
        call putstr
        add esp, 4
        call quit                   ; syscall _exit
