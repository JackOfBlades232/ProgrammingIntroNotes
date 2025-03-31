#!/usr/bin/gcl -f

(defun pr (var)
    (princ var)
    (terpri)
)

; setq is a specform, sets val of second to first (first is not computed)
(setq x 5)
(setq x (+ x 1))

(pr x)

; 'globvars' may not be accessible if made w/ setq
(defvar *someglobvar* 4)

(defun gprinter ()
    (setq locvar (+ *someglobvar* 1))
    (setq *someglobvar* (* locvar 2))
    (princ *someglobvar*)
    (terpri)
)

(gprinter)
(gprinter)
(gprinter)

; we can also replace contents of a dotted pair
(setq y '(1 2 3))
(rplaca y 10)
(rplacd (cdr y) 30) ; -> (10 2 . 30)
(pr y)

; with rpla* we can cycle a list (by replacing pointing tail to itself)
(setq z '(1 2 3))
(rplacd (cdr (cdr z)) z)
; (pr z)

; to copy a list, we need to manually pull out every car
(defun listcopy (lst)
    (cond
        ((atom lst) lst)
        (t (cons (car lst) (listcopy (cdr lst))))
    )
)

(setq a '(1 2 3))
(setq b (listcopy a))
(rplaca a 4)
(rplaca b 5)
(pr a)
(pr b)

; there is also setf -- weird thing, takes frist arg as a ref
(setf (car a) 10) ; same as rplaca
(pr a)

; this can be used for arrays (not lists), strings to set elems
(setq str "foo")
(setf (char str 1) #\i)
(pr str)

; more weird, this gives a ref to the place where the symbol has an associated
; function
(setf (symbol-function 'mycar) (symbol-function 'car))
(pr (mycar a))
