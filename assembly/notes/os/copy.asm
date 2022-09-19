%include "kernel.inc"

section .data
%ifdef OS_FREEBSD               ; open file to write flag diffs for linux/bsd
openwr_flags    equ 601h
%else   ; assume it is linux
openwr_flags    equ 241h
%endif
; messages for error situations
helpmsg db 'Usage: copy <src> <dest>', 10
helplen equ $-helpmsg
err1msg db "Couldn't open source file for reading", 10
err1len equ $-err1msg
err2msg db "Couldn't open destination file for writing", 10
err2len equ $-err2msg

section .bss
buffer  resb 4096               ; data buffer
bufsize equ $-buffer            ; buffer size
fdsrc   resd 1                  ; source file descriptor
fddest  resd 1                  ; destination file descriptor
argc    resd 1                  ; cli arg num
argvp   resd 1                  ; cli arg pointer array beginning

section .text
global _start
_start:
        pop dword [argc]
        mov [argvp], esp
        cmp dword [argc], 3     ; check if 2 args were passed (src/dest)
        je .args_cnt_ok
        kernel 4, 2, helpmsg, helplen   ; else, put error msg in stderr
        kernel 1, 1             ; and exit with error code
.args_cnt_ok:
        mov esi, [argvp]        ; now, try to open first file for reading
        mov edi, [esi+4]        ; file name adr in edi now
        kernel 5, edi, 0        ; open syscall, O_RDONLY
        cmp eax, -1             ; check for error
        jne .src_open_ok
        kernel 4, 2, err1msg, err1len   ; same deal with error
        kernel 1, 2             ; now with code 2
.src_open_ok:
        mov [fdsrc], eax
        mov edi, [esi+8]        ; now second file, esi already set
        kernel 5, edi, openwr_flags, 0666q  ; set rights to rwrwrw
        cmp eax, -1
        jne .dest_open_ok
        kernel 4, 2, err2msg, err2len
        kernel 6, [fdsrc]       ; have to close src file 
        kernel 1, 3
.dest_open_ok:
        mov [fddest], eax
.again: kernel 3, [fdsrc], buffer, bufsize  ; main copy cycle
        cmp eax, 0              ; check if eof or error
        jle .end_of_file
        kernel 4, [fddest], buffer, eax     ; write the bytes read to dest
        jmp .again
.end_of_file:                   ; if all successful, close both files and ret 0
        kernel 6, [fdsrc]
        kernel 6, [fddest]
        kernel 1, 0
