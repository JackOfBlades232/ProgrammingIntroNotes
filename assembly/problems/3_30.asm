%macro CHAR2NUM 1           ; macro takes a string literal and creates arr of
  %ifstr %1                 ;   numbers with char codes as 4-byte numbers
    %strlen %%len %1       
    %assign %%i 1         
    %rep %%len
      %substr %%sym %1 %%i
      dd %%sym
      %assign %%i %%i+1
    %endrep
  %endif
%endmacro

section .data
wrd     CHAR2NUM "Beautiful day"    ; testing macro
