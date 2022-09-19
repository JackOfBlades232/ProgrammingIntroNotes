%include "kernel.inc"

section .data
msg     db "Hello, world!", 10
msg_len equ $-msg

section .text
global _start
_start: kernel 4, 1, msg, msg_len   ; write syscall
        kernel 1, ecx               ; exit with write exit code
