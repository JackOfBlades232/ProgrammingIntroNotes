%macro GOTO 1-*             ; macro takes list of labels and goes to ind eax
        dec eax             ; make eax a normal index
        cmp eax, %0
        ja %%quit           ; if index out of scope, jump out
  %assign %%i 0  
  %rep %0
    %ifid %1               ; if param not identifier, do nothing
        cmp eax, %%i        ; check if this index is eax
        jz %1               ; if so, jump to label
    %endif
    %rotate 1               ; then, put next label in place of first
    %assign %%i %%i+1       ; and increment index
  %endrep
%%quit: inc eax             ; restore eax
%endmacro

section .text
l0:     nop
l1:     nop
l2:     nop
        mov eax, 2          ; test macro
        GOTO l0, l1, l2
