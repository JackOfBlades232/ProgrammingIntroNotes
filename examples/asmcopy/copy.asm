%include "kernel.inc"

global	_start
extern	transfer
extern	msg_help
extern	msg_errsrc
extern	msg_errdst
extern	message_and_quit
extern	open_or_die

section	.text
_start:
	mov ebp, esp
	cmp dword [ebp], 3
	je .args_ok
	mov eax, msg_help
	jmp message_and_quit
.args_ok:
	; open the source file
	mov eax, [ebp+8]
	mov ecx, 0        ; O_RDONLY
	mov edx, msg_errsrc
        call open_or_die
	mov esi, eax

	; now open the destination
	mov eax, [ebp+12]
	mov ecx, openwr_flags
	mov edx, msg_errdst
        call open_or_die
	mov edi, eax

	; transfer the content
	push esi
	push edi
	call transfer
	add esp, 8

        ; close both files
	kernel 6, esi
	kernel 6, edi
        ; quit
	kernel 1, 0
