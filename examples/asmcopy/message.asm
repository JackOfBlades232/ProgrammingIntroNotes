%include "kernel.inc"

global msg_help
global msg_errsrc
global msg_errdst
global message_and_quit

section		.data

helpmsg		db 'Usage: copy <src> <dest>', 10
helplen		equ $-helpmsg
err1msg		db "Couldn't open source file for reading", 10
err1len		equ $-err1msg
err2msg		db "Couldn't open destination file for writing", 10
err2len		equ $-err2msg

msg_array	dd helpmsg, helplen, err1msg, err1len, err2msg, err2len

msg_help	equ 0
msg_errsrc	equ 1
msg_errdst	equ 2


section		.text

;; in: eax = message_id
message_and_quit:
		mov ecx, [msg_array + eax*8]
		mov edx, [msg_array + eax*8 + 4]
		kernel	4, 1, ecx, edx
		kernel	1, 1
