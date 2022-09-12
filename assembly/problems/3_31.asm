;%define WFILL               ; symbol for word-filling
%define DFILL               ; symbol for dword-filling
;%define QFILL               ; symbol for qword-filling

%macro CHAR2NUM 1           ; macro takes a string literal and creates arr of
  %ifstr %1                 ;   numbers with char codes as 4-byte numbers
    %strlen %%len %1       
    %assign %%i 1         
    %rep %%len
      %substr %%sym %1 %%i
      %ifdef WFILL
        dw %%sym 
      %elifdef DFILL
        dd %%sym
      %elifdef QFILL
        dq %%sym
      %else
        db %%sym
      %endif
      %assign %%i %%i+1
    %endrep
  %endif
%endmacro

section .data
wrd     CHAR2NUM "Beautiful day"    ; testing macro
