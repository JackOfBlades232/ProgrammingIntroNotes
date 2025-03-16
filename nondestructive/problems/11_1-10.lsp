#!/usr/bin/gcl -f

(defun output (task what)
    (princ task)
    (princ ": ")
    (princ what)
    (terpri)
)

; 11_1
(defun range (n)
    (defun range-impl (n m)
        (cond
            ((< (- m n) 0) nil)
            (t (cons n (range-impl (+ n 1) m)))
        )
    )
    (cond
        ((not (integerp n)) 'wrong_input)
        (t (range-impl 1 n))
    )
)

(output "11_1" (range 4))

; 11_2 TODO
