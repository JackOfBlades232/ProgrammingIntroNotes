;; 3_48/main.asm ;;
global _start
extern calc_number_of_reps
extern trig
extern write_chars
extern read_float
extern write_float
extern write_nl
extern write_string
extern open_file_wr
extern close_file

%macro read_cli_float 2         ; params: offset from esp (int/reg), err label
        mov eax, [esp+%1]       ; then, min (to st1)
        call read_float
        cmp eax, 0
        jnz %2
%endmacro 

%macro call_open_file 3         ; params: file name adr, fd adr, err label
        push dword %1
        call open_file_wr
        add esp, 4

        cmp eax, -1
        jz %3

        mov [%2], eax
%endmacro

%macro call_close_file 1        ; params: fd
        push dword %1
        call close_file
        add esp, 4
%endmacro

%macro call_write_nl 1          ; params: fd
        push dword %1
        call write_nl
        add esp, 4
%endmacro

%macro call_write_str 3         ; params: adr, len, fd
        push dword %1           
        push dword %2
        push dword %3
        call write_string
        add esp, 12
%endmacro

%macro call_write_chars 3       ; params: adr, n, fd
        push dword %1           
        push dword %2
        push dword %3
        call write_chars
        add esp, 12
%endmacro

%macro write_horiz_line 3       ; params: char, num chars, fd 
        call_write_chars %1, %2, %3 ; first, write line of '-'
        call_write_nl %3        ; and new line
%endmacro 

%macro write_float_elem 5       ; params: dgts adr, dgts ln
        mov eax, %1             ;   pref adr, pref ln, fd
        mov ecx, %2
        call write_float
        fstp st0

        call_write_str %3, %4, %5
        call_write_str %1, %2-1, %5 ; discard #0
%endmacro

section .bss
fd      resd 1
dgts    resb 19
dgts_ln equ $-dgts
reps    resd 1
incr    resq 1

section .data
pref    db "| "
pref_ln equ $-pref
sep     db " | "
sep_ln  equ $-sep
suf     db " |"
suf_ln  equ $-suf
line_ch db '-'
line_ln equ 5*(2+dgts_ln)+1
spc     db ' '
ang_wrd db "angle"
ang_ln  equ $-ang_wrd
ang_spc equ dgts_ln-ang_ln-1
sin_wrd db "sin"
sin_ln  equ $-sin_wrd
sin_spc equ dgts_ln-sin_ln-1
cos_wrd db "cos"
cos_ln  equ $-cos_wrd
cos_spc equ dgts_ln-cos_ln-1
tan_wrd db "tan"
tan_ln  equ $-tan_wrd
tan_spc equ dgts_ln-tan_ln-1
ctn_wrd db "ctan"
ctn_ln  equ $-ctn_wrd
ctn_spc equ dgts_ln-ctn_ln-1

section .text
        ; preparations
_start: cmp dword [esp], 5          ; must be cli args: file name,
        jnz .err                    ;   min angle, max angle, step
        finit

        ; open file
        call_open_file [esp+8], fd, .err

        ; read min/max angles and step to memory;
        read_cli_float 20, .err     ; step first (to st2)
        read_cli_float 12, .err     ; then, min (to st1)
        read_cli_float 16, .err     ; last, max (to st0)

        ; calculate number of entries
        call calc_number_of_reps    ; number of reps/err code to eax
        cmp eax, 0
        jz .err
        mov [reps], eax

        ; save off increment and remove it from stack
        fxch st1
        fstp qword [incr] 

        ; header
        write_horiz_line [line_ch], line_ln, [fd]

        call_write_str pref, pref_ln, [fd]
        call_write_str ang_wrd, ang_ln, [fd]
        call_write_chars [spc], ang_spc, [fd]

        call_write_str sep, sep_ln, [fd]
        call_write_str sin_wrd, sin_ln, [fd]
        call_write_chars [spc], sin_spc, [fd]

        call_write_str sep, sep_ln, [fd]
        call_write_str cos_wrd, cos_ln, [fd]
        call_write_chars [spc], cos_spc, [fd]

        call_write_str sep, sep_ln, [fd]
        call_write_str tan_wrd, tan_ln, [fd]
        call_write_chars [spc], tan_spc, [fd]

        call_write_str sep, sep_ln, [fd]
        call_write_str ctn_wrd, ctn_ln, [fd]
        call_write_chars [spc], ctn_spc, [fd]

        call_write_str suf, suf_ln, [fd]
        call_write_nl [fd]

        ; main cycle: calc trig funcs and output
.again: write_horiz_line [line_ch], line_ln, [fd]
        ; calc all 4 trig funcs (are in reverse in stack), and output them
        call trig

        fld st4                     ; angle to top
        write_float_elem dgts, dgts_ln, pref, pref_ln, [fd]
        
        mov ebx, 4                  ; now, empty stack to file (5 numbers)
.line_lp:
        write_float_elem dgts, dgts_ln, sep, sep_ln, [fd]
        dec ebx
        cmp ebx, 0
        jnz .line_lp

        call_write_str suf, suf_ln, [fd]
        call_write_nl [fd]

        ; now, increment angle
        fadd qword [incr]

        ; and check if reps left to continue/break
        dec dword [reps]
        cmp dword [reps], 0
        jnz .again

        ; finally, cap off with line
        write_horiz_line [line_ch], line_ln, [fd]

        ; if all ok, 0 in exit code, else 1
.ok:    mov ebx, 0
        jmp .close

.err:   mov ebx, 1

        ; close file
.close: call_close_file [fd]
        
        ; terminate with code earlier set
        mov eax, 1
        int 80h
