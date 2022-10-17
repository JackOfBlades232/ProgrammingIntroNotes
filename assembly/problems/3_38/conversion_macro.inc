; params: char, success label, error label
%macro char2num 3
        cmp byte %1, '0'        ; decide if digit is valid and convert
        jb %3
        cmp byte %1, '9'
        ja %%check_A
        sub byte %1, '0'
        jmp %2
%%check_A:
        cmp byte %1, 'A'
        jb %3
        cmp byte %1, 'Z'
        ja %3
        sub byte %1, 'A'-10     ; A stands for 10
%endmacro

; params : digit number (no error han%1ing), jump-out label
%macro num2char 2
        cmp %1, 9
        ja %%conv_A
        add %1, '0'
        jmp %2
%%conv_A:
        add %1, 'A'-10
%endmacro
