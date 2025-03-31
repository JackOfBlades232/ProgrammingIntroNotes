#!/usr/bin/gcl -f

(princ (* (+ 3 7) 5 (- 6 4)))
(princ (quote (1 2 3))) ; quote is a special form -- returns arg as value
(princ '(1 2 3)) ; same
(princ (cons 1 (cons 2 nil))) ; cons -- makes a . pair, a func for a change
(princ '(1 . (2 . ()))) ; same
(princ (list 1 2)) ; same
(princ (car '(1 2 3))) ; first
(princ (cdr '(1 2 3))) ; second
(princ (if (< 1 2) 'yas 'nah)) ; ternary if specform
(princ
    (cond ; kinda like a switch -- goes until matches
        ((> 1 3) '(1 2 3))
        ((eq 1 'a) '(1 2)) ; same object
        ((eql 1 'a) '(1)) ; eq or same number or char
        ((equal 1 'a) '()) ; eql recursively into dotted pairs
        (t 'faild)
    )
)
(terpri)
