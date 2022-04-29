%include "kernel.inc"

global open_or_die
extern message_and_quit

section .text

;; in: eax = file name ptr, ecx = mode, edx = message_id
;; out: eax = file descriptor
open_or_die:	push edx
		kernel 5, eax, ecx, 0666o
		pop edx
		cmp eax, -1
		jne .open_ok
		mov eax, edx
		jmp message_and_quit
.open_ok:	ret
