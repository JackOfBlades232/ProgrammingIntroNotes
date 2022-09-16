; some file open flags for syscall open
%define O_RDONLY 000h       ; only read
%define O_WRONLY 001h       ; only write
%define O_RDWR 002h         ; read and write
%define O_CREAT 040h        ; can create new
%define O_EXCL 080h         ; require create new
%define O_TRUNC 200h        ; if exists, del whats in it
%define O_APPEND 400h       ; if exists, append to end
%assign O_REWRITE O_WRONLY|O_CREAT|O_TRUNC  ; if exitst, rewrite, else create

global _start

section .data
arr_len equ 1024
file_nm db "test_file.txt", 0

section .bss
arr     resb 1024
real_ln resd 1
file_d  resd 1

section .text
_start: mov eax, 3          ; 3 for syscall read
        mov ebx, 0          ; 0 for std input
        mov ecx, arr        ; second and third params like in write
        mov edx, arr_len
        int 80h

        cmp eax, 0          ; check for err (if eax < 0 then err, if = 0 then
        jle quit            ;   eof, else number of bytes read)
        mov [real_ln], eax  ; else, save off read bytes to mem

o_file: mov eax, 5          ; 5 for syscall open (will redirect input there)
        mov ebx, file_nm    ; first param filename, must end with char #0
        mov ecx, O_REWRITE  ; second param is mode
        mov edx, 0666q      ; third param is access rigths (like chmod)
        int 80h
        mov [file_d], eax   ; returns the descriptor num/error code
        
        cmp eax, 0          ; check if open ended with err (correct would be to
        jl quit             ;   check between fffff000h and -1)

        mov eax, 4          ; so ill write to the file
        mov ebx, [file_d]   
        mov ecx, arr
        mov edx, [real_ln]
        int 80h

        mov eax, 6          ; now, close file, syscall 6
        mov ebx, [file_d]   ; with only descriptor as param
        int 80h             ; descriptor already in ebx

        mov eax, 20         ; 20 for syscal getpid (process id to eax)
        int 80h

        mov eax, 64         ; 64 for syscal getppid (parent process id to eax)
        int 80h             ;   (the process that ran this one)

quit:   mov eax, 1          ; exit seq
        mov ebx, 0          ; code 0 for sake of simplicity
        int 80h

        ; making this unreachable
        mov eax, 37         ; 37 for syscall kill
        mov ebx, 1111       ; process id to ebx (here, random)
        mov ecx, 15         ; signal number to ecx, 15-SIGTERM, 9-SIGKILL
        int 80h
