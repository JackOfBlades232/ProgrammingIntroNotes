#!/usr/bin/gcl -f

(defun pr (var)
    (princ var)
    (terpri)
)

; it also has reduce by spec, but these are implementable
(defun lreduce (fun lst init)
    (cond
        ((null lst) init)
        (t
            (let ((next (funcall fun init (car lst))))
                (lreduce fun (cdr lst) next)
            )
        )
    )
)
(defun rreduce (fun lst init)
    (cond
        ((null lst) init)
        (t (funcall fun (car lst) (rreduce fun (cdr lst) init)))
    )
)

(pr (rreduce #'cons '(1 2 3) ())) ; copy of 1 2 3
(pr (lreduce #'cons '(1 2 3) ())) ; inverted pair sequence

(pr (lreduce #'(lambda (a b) (cons b a)) '(1 2 3) ())) ; reverses the list
(pr (rreduce #'(lambda (a b) (cons b a)) '(1 2 3) ())) ; inverted-reverted))

(defun max-in-list (ls)
    (lreduce #'(lambda (x y) (if (< x y) y x)) (cdr ls) (car ls))
)

(pr (max-in-list '(1 2 3)))

(defun ins2list (i lst)
    (cond
        ((null lst) (cons i ()))
        ((< i (car lst)) (cons i lst))
        (t (cons (car lst) (ins2list i (cdr lst))))
    )
)
(defun redsort (lst)
    (rreduce #'ins2list lst ())
)
(defun redsort2 (lst)
    (lreduce #'(lambda (ls el) (ins2list el ls)) lst ())
)

(pr (redsort '(4 1 6 8 3 11)))
(pr (redsort2 '(4 1 6 8 3 11)))

(defun redmap (fun lst)
    (rreduce #'(lambda (x res) (cons (funcall fun x) res)) lst ())
)
(defun select (lst pred)
    (rreduce
        #'(lambda (x ls) (if (funcall pred x) (cons x ls) ls))
        lst ()
    )
)

(pr (redmap #'- '(1 2 3)))
(pr (select '(1 2 "abc" t 15) #'numberp))

