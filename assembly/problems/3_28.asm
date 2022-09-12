; macro for creating and filling arr in mem with a+n*b until c (a, b, c params)
%macro FILLRANGE 3          ; 3 params: init, step and limit
  %assign %%range %3-%1
  %if %2 > 0
    %assign %%dir 1
  %elif %2 < 0
    %assign %%dir -1
  %else
    %assign %%dir 0 
  %endif
  %assign %%limit %3*%%dir
  %if %%dir != 0 && %%range*%%dir >= 0  ; check if macro will be non inf or empty
    %assign %%i %1
    %rep %%range*%%dir+1
        dw %%i              ; reserve mem for next number
      %assign %%i %%i+%2    ; increment by step
      %if %%i*%%dir > %%limit   ; check if exceeded limit
        %exitrep 
      %endif
    %endrep
  %endif
%endmacro

section .data
range0  FILLRANGE 10, 2, 21 ; testing macro
range1  FILLRANGE 10, -1, 5 ; testing macro
range2  FILLRANGE 10, 2, 10 ; testing macro
range3  FILLRANGE 10, 0, 21 ; testing macro
range4  FILLRANGE 10, -1, 21    ; testing macro
