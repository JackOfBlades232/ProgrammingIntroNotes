%include "../../stud_io.inc"
global _start
extern read_num
extern write_num
extern input_str
extern output_str

section .bss
digits  resb 10             ; array of digits for output

section .text
_start: nop                 ; *PLACEHOLDER*
        FINISH              ; exit with macro
