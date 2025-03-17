#!/usr/bin/gcl -f

(defun output (task what)
    (princ task)
    (princ ": ")
    (princ what)
    (terpri)
)

; 11_1
(defun range-impl (n m)
    (cond
        ((< (- m n) 0) nil)
        (t (cons n (range-impl (+ n 1) m)))
    )
)
(defun range (n)
    (cond
        ((not (integerp n)) 'wrong_input)
        (t (range-impl 1 n))
    )
)

(output "11_1" (range 4))

; 11_2
(defun take-first (expr n)
    (cond
        ((not (integerp n)) 'wrong_input)
        ((<= n 0) nil)
        (t (cons (car expr) (take-first (cdr expr) (- n 1))))
    )
)

(output "11_2" (take-first '((1 2 3) "asd" + (aaa) () t () () () () ()) 8))

; 11_3
(defun sum-integers (lst)
    (cond
        ((null lst) 0)
        ((integerp lst) lst)
        ((not (listp lst)) 0)
        (t (+ (sum-integers (car lst)) (sum-integers (cdr lst))))
    )
)

(output "11_3" (sum-integers '(1 2 "" (3 nil (4 5 t) 6 ((7 8)) 9 9))))

; 11_4
(defun flatten-atoms-impl (lst res)
    (cond
        ((atom lst) res)
        ((atom (car lst)) (cons (car lst) (flatten-atoms-impl (cdr lst) res)))
        ((listp (car lst)) (flatten-atoms-impl (car lst) (flatten-atoms-impl (cdr lst) res)))
        (t (flatten-atoms-impl (cdr lst) res))
    )
)
(defun flatten-atoms (lst)
    (flatten-atoms-impl lst ())
)

(output "11_4" (flatten-atoms '(1 2 (3 (4 5) 6 ((7 8)) 9 9))))
