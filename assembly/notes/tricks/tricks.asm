%include "../../stud_io.inc"
global _start

section .bss
eight   dq 2.2              ; reserving 8-byte quadroword
ten     dt 1.01             ; and 10-byte word (for floats only)
        ; floats can also be used with dd
four    equ 4               ; binding const 4 to mark four
msg     db "Hello and welcome", 10, 0   ; equ can be used for storing arr len
msglen  equ $-msg           ; $ means rel adr that became current after writing
                            ; the string, so $-msg will be computed while 
                            ; assembling and will equal the arr length
stars   times 4096 db '*'   ; times repeats a command n times (this'll be arr)
        ; incbin for creating mem space filled with data from external file
        ; there are symbol and string consts. String -- arrs of bytes. Sym --
        ;   up to 4 bytes, treated as a number (empty filled with 0oes)
        ; Know, that 1 and 1.0 are entirely different constants (in binary)
frac    dt 1.0e-5           ; another way of writing consts (a dot's necessary)
        ; instead of consts you can write expr of constants and:
        ; +-, /% (int uns), // %% (int signed), &|^(and or xor), << >> (shl/r)
        ; - + as unary, ~ -- bitwise negation and ()
        ; / // % %% require spaces
        ; REMEMBER: assembler reads code twice, first counts mem and assigns
        ; labels. On second, generates machine code (so you can jump to labels
        ; defined later). however, if you use expressions with labels which are
        ; introduced later in something like memory reservations, you cant do
        ; it on first read, so you can only use expl consts and labels defined
        ; before the expr. Such exprs are called critical. (some of them: times
        ; , res..., d..., and things like mov eax, [ebx+c], because machine 
        ; word will have different len depending on c. Workaround: mov eax,
        ; [ebx + dword label]

section .data
deight  resq 1              ; =|=
dten    rest 1  

section .text
_start: mov eax, \
        ecx                 ; how to break lines

        ; empty line is ignored, comment line is not
_$#@~.?:                    ; allowed symbols in labels, first only _, ?, .
        nop
        ; labels are register-sensitive
        MOV eax, 1          ; command names are case-insensitive
        PUTCHAR 10          ; and macro names are sensitive

