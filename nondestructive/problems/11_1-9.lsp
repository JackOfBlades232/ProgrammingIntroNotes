#!/usr/bin/gcl -f

(defun output (task what)
    (princ task)
    (princ ": ")
    (princ what)
    (terpri)
)

(defun outputc (task what comment)
    (princ task)
    (princ ": ")
    (princ what)
    (princ " (")
    (princ comment)
    (princ ")")
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
        ((null expr) nil)
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

; 11_5
(defun properlist (lst)
    (cond
        ((null lst) t)
        ((atom lst) nil)
        (t (properlist (cdr lst)))
    )
)

(output "11_5" (properlist '(1 . (2 . 3))))

; 11_6
(defun is-even-cnt-list (lst)
    (cond
        ((null lst) t)
        ((not (listp lst)) nil)
        (t (not (is-even-cnt-list (cdr lst))))
    )
)

(output "11_5" (is-even-cnt-list '(1 "asd" () ((9 9)))))

; 11_7
(outputc "11_7"
    (take-first '((1 2 3) "asd" + (aaa) () t () () () () ()) 8)
    "how is this different from 11_2? The wording of 11_2 is kinda ambiguous"
)

; 11_8
(defun list-iter (lst)
    (setq state lst)
    (setq ll
        #'(lambda ()
            (setq saved (car state))
            (setq state (cdr state))
            saved
        )
    )
    (cond
        ((listp lst) #'(lambda () (if (null lst) nil (funcall ll))))
        (t nil)
    )
)

(setq li (list-iter '(1 2 "a")))
(output "11_8" (funcall li))
(output "11_8" (funcall li))
(output "11_8" (funcall li))
(output "11_8" (funcall li))
(output "11_8" (funcall li))

; 11_9
(defun rev-list (lst)
    (setq ret nil)
    (mapcar #'(lambda (elem) (setq ret (cons elem ret))) lst)
    ret
)

(output "11_9" (rev-list '(1 2 3 4 5 6 7 8 (9 9))))
