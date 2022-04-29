%include "kernel.inc"

global transfer

bufsize		equ 1024

section .text

; [ebp+8] dest. fd;  [ebp+12] src. fd; [ebp-bufsize] buffer
transfer:	push ebp
		mov ebp, esp
		sub esp, bufsize
		push ebx
		mov ebx, ebp
		sub ebx, bufsize	; buffer addr. in ebx
.again:		kernel 3, [ebp+12], ebx, bufsize
		cmp eax, 0
		jle .quit
		kernel 4, [ebp+8], ebx, eax
		jmp .again
.quit:		pop ebx
		mov esp, ebp
		pop ebp
		ret
