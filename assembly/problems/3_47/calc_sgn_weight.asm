;; 3_47/calc_sgn_weight.asm ;;
global calc_sgn_weight

section .text
; proc calc_sgn_weight : calculates weight of a sign char for Dijkstra alg
;   #0 : 0, '(': 1, '+','-': 2, '*','/': 3, ')': 4, else: -1
; char==al : weight->cl
calc_sgn_weight:
        cmp al, 0
        jz .zero
        cmp al, '('
        jz .one
        cmp al, '+'
        jz .two
        cmp al, '-'
        jz .two
        cmp al, '*'
        jz .three
        cmp al, '/'
        jz .three
        cmp al, ')'
        jz .four

        mov cl, -1
        jmp .quit

.zero:  xor cl, cl
        jmp .quit
.one:   mov cl, 1
        jmp .quit
.two:   mov cl, 2
        jmp .quit
.three: mov cl, 3
        jmp .quit
.four:  mov cl, 4

.quit:  ret
