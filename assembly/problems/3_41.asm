;; 3_41.asm ;;
%include "useful_macros.inc"

global _start
extern read_hex

section .data
%ifdef OS_FREEBSD               
openwr_flags    equ 601h
%else   ; assume it is linux
openwr_flags    equ 241h
%endif
arg_err db "Please provide source file, dest file and number of blocks!"
arg_ln  equ $-arg_err
num_err db "Invalid number of blocks to copy! Provide a positive hex number."
num_ln  equ $-num_err
src_err db "Couldn't open souce file!"
src_ln  equ $-src_err
dst_err db "Couldn't open destination file!"
dst_ln  equ $-dst_err

section .bss
buffer  resb 4096
bufsize equ $-buffer
src_fd  resd 1                  ; descriptor for source file
dst_fd  resd 1                  ; and for dest file
block_c resd 1                  ; number of blocks to be copied

section .text
_start: cmp dword [esp], 4      ; check if 2 files and number N is provided
        jz .read_num            ; if all ok, proceed, else error
        kernel 4, 2, arg_err, arg_ln
        kernel 1, 1
.read_num:
        pcall read_hex, [esp+16]
        mov [block_c], eax
        cmp cl, 0
        jnz .num_err
        cmp eax, 0
        jnz .open_src
.num_err:
        kernel 4, 2, num_err, num_ln
        kernel 1, 1
.open_src:
        mov edi, [esp+8]        ; src filename into edi
        kernel 5, edi, 0        ; open to read
        cmp eax, -1             ; check if success
        ja .src_ok
        kernel 4, 2, src_err, src_ln
        kernel 1, 1
.src_ok:
        mov [src_fd], eax
        mov edi, [esp+12]   
        kernel 5, edi, openwr_flags  
        cmp eax, -1             ; check if success
        ja .dst_ok
        kernel 4, 2, dst_err, dst_ln
        kernel 1, 1
.dst_ok:
        mov [dst_fd], eax
.again: cmp dword [block_c], bufsize
        jl .read_rest
        kernel 3, [src_fd], buffer, bufsize     ; read bytes
        jmp .write
.read_rest:
        kernel 3, [src_fd], buffer, [block_c]
.write:
        cmp eax, 0
        jle .quit               ; deal with error?
        sub [block_c], eax
        kernel 4, [dst_fd], buffer, eax
        ; deal with error?
        cmp dword [block_c], 0
        jg .again
.quit:  kernel 6, [src_fd]
        kernel 6, [dst_fd]
        kernel 1, 0             ; if not jumped to err, exit with code 0
