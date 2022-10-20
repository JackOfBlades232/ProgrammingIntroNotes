;; debug_tools.inc ;;

%macro debug_print_number 5           ; params: num, msg/ln, new line msg/ln
        push ebx
        mov ebx, %1
%%lp:   dec ebx
        cmp ebx, -1
        jz %%f
        kernel 4, 1, %2, %3
        jmp %%lp
%%f:    kernel 4, 1, %4, %5
        pop ebx
%endmacro
