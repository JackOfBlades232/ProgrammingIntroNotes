%include "../stud_io.inc"
global _start

section .text
_start: xor edx, edx        ; reset line num counter
ln_lp:  xor ecx, ecx        ; reset string char counter
chr_lp: GETCHAR             ; read next char from string
        cmp eax, -1         ; check eof
        jz outp             ; if eof, go to ouput text
        cmp eax, 10         ; check eoln
        jz ln_fin           ; if eoln, go to handle line end
        push eax            ; push char to stack
        inc ecx             ; increase char counter
        jmp chr_lp          ; repeat char loop
ln_fin: push ecx            ; push line length to stack
        inc edx             ; increase line counter
        jmp ln_lp           ; repeat line loop
outp:   ; thoughts: push all lengths to stack (with ebp), and go through stack
        cmp edx, 0          ; check if no lines were input
        jz quit             ; if so, quit
        mov ebp, esp        ; put stack base on stack top (first length)
        mov ecx, edx        ; put line count into ecx for loop
        dec ecx             ; we have already pushed last len to stack
        jecxz out_st        ; if ecx is 0, skip stack filling
st_lp:  mov ebx, [ebp]      ; move next length to edx
        lea ebp, [ebp+4*ebx+4]  ; shift ebp by the value ((len + 1) * 4)
        push dword [ebp]    ; push next line length to stack
        loop st_lp          ; repeat for every line, now ebp is above line1
out_st: pop ebx             ; pop first line length to ebx
out_lp: xor ecx, ecx        ; zero out counter
lnout_lp:
        cmp ecx, ebx        ; check if already line len
        jge lnout_fin          ; if so, jump out
        inc ecx             ; increment counter
        PUTCHAR byte [ebp+4*ecx]    ; output the char in stack
        jmp lnout_lp        ; repeat inner loop
lnout_fin: 
        PUTCHAR 10          ; newline output
        dec edx             ; check off outputted line
        cmp edx, 0          ; check if it was last line
        jz quit             ; if so, quit
        pop ebx             ; pop next line length
        mov eax, ebx        ; copy ebx to eax
        inc eax             ; add 1 to eax
        shl eax, 2          ; and mult it by 4
        sub ebp, eax        ; shift ebp to next line len
        jmp out_lp          ; repeat for next line
quit:   FINISH              ; exit with macro
