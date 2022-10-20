;; 3_41.asm ;;

%ifdef DEBUG
  %macro debug_print_num 3      ; params: number, message, msg len
        push ebx
        mov ebx, %1
%%lp:   kernel 4, 1, %2, %3
        dec ebx
        cmp ebx, 0
        jg %%lp
        pop ebx
  %endmacro
%endif

%include "useful_macros.inc"

global _start
extern read_hex

section .data
%ifdef OS_FREEBSD               
openwr_flags    equ 601h
%else   ; assume it is linux
openwr_flags    equ 241h
%endif
arg_err db "Please provide source file, dest file and number of blocks!", 10, 0
arg_ln  equ $-arg_err
num_err db "Invalid number of blocks! Provide a positive hex number.", 10, 0
num_ln  equ $-num_err
src_err db "Couldn't open souce file!", 10, 0
src_ln  equ $-src_err
dst_err db "Couldn't open destination file!", 10, 0
dst_ln  equ $-dst_err
%ifdef DEBUG
dbg_msg db "DEBUG", 10, 0
dbg_ln  equ $-dbg_msg
%endif

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
        jne .src_ok
        kernel 4, 2, src_err, src_ln
        kernel 1, 1
.src_ok:
        mov [src_fd], eax
        mov edi, [esp+12]   
        kernel 5, edi, openwr_flags  
        cmp eax, -1             ; check if success
        jne .dst_ok
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
        jle .quit               
        sub [block_c], eax
        kernel 4, [dst_fd], buffer, eax
%ifdef DEBUG
        debug_print_num eax, dbg_msg, dbg_ln
%endif
        cmp dword [block_c], 0
        jg .again
.quit:  kernel 6, [src_fd]
        kernel 6, [dst_fd]
        kernel 1, 0             ; if not jumped to err, exit with code 0
