#!/usr/bin/gcl -f

(defun sqe-discrim (a b c) ; specform that defines a func
    (- (* b b) (* 4 a c))  ; then symbol-name, then symbol list for param names
) ; defun has a side effect -- ties name symbol with the func

(defun listlen (lst)
    (cond
        ((null lst) 0)
        ((atom lst) nil)
        ((atom (cdr lst)) 1)
        (t (+ 1 (listlen (cdr lst))))
    )
)

(defun listrev_impl (lst res)
    (cond
        ((null lst) res)
        (t (listrev_impl (cdr lst) (cons (car lst) res)))
    )
)
(defun listrev (lst) (listrev_impl lst nil))

(princ (sqe-discrim 2 2 1))
(terpri)
(princ (listlen '(1 2 3)))
(terpri)
(princ (listrev '(1 2 3)))
(terpri)
