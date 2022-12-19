;; 3_42.asm ;;
%include "useful_macros.inc"

%ifdef DEBUG
  %include "debug_tools.inc"
%endif

global _start
%ifdef DEBUG
extern write_hex
%endif

section .data
%ifdef OS_FREEBSD               
openwr_flags    equ 601h
%else   ; assume it is linux
openwr_flags    equ 241h
%endif
dst_nm  db "result.dat", 0
dst_ln  equ $-dst_nm
dst_err db "Couldn't open destination file!", 10, 0
err_ln  equ $-dst_err
%ifdef DEBUG
dbg_msg db "@ ", 0
dbg_ln  equ $-dbg_msg
nl      db 10, 0
nl_ln   equ $-nl
%endif

section .bss
buffer  resb 1024
bufsize equ $-buffer
fd      resd 1
ln_cnt  resd 1
chr_cnt resd 1
max_ln  resd 1

section .text
_start: mov dword [ln_cnt], 0           ; prepare counters
        mov dword [chr_cnt], 0
        mov dword [max_ln], 0
        xor ebx, ebx                    ; cur line len in ebx
.input_lp:
        kernel 3, 0, buffer, bufsize    ; read next line from stdin
        cmp eax, 0                      ; and check for eof
        jle .error_or_eof
        mov esi, buffer
        mov ecx, eax                    ; num chars read into ecx
.nest_lp:
        cmp byte [esi], 10
        jz .handle_nl
        inc ebx
        jmp .nest_lp_fin
.handle_nl:
        inc dword [ln_cnt]              ; update counters on line end
        add [chr_cnt], ebx
        cmp ebx, [max_ln]
        jbe .reset_char_counter
        mov [max_ln], ebx
.reset_char_counter:
        xor ebx, ebx
.nest_lp_fin:
        inc esi
        loop .nest_lp
        jmp .input_lp
.error_or_eof:
        kernel 5, dst_nm, openwr_flags, 0666q   ; open result.dat and set rw
        cmp eax, -1
        jne .dest_open_ok
        kernel 4, 2, dst_err, err_ln
        kernel 1, 1
.dest_open_ok:
        mov [fd], eax
%ifdef DEBUG
        debug_print_number [ln_cnt], dbg_msg, dbg_ln, nl, nl_ln
        debug_print_number [chr_cnt], dbg_msg, dbg_ln, nl, nl_ln
        debug_print_number [max_ln], dbg_msg, dbg_ln, nl, nl_ln

%endif
        kernel 4, [fd], ln_cnt, 4     ; now write 4-byte numbers to dest
        kernel 4, [fd], chr_cnt, 4 
        kernel 4, [fd], max_ln, 4 
        kernel 6, [fd]
        kernel 1, 0
