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

(defun max_in_list (lst)
    (lreduce #'(lambda (x y) (if (< x y) y x)) (cdr lst) (car lst))
)

(defun ins2list (i list)
    (cond
        ((null list) (cons i ()))
        ((< i (car list)) (cons i list))
        (t (cons (car list) (ins2list i (cdr list))))
    )
)

(defun redsort (lst) (rreduce #'ins2list lst ()))

(defun redsort2 (lst)
    (lreduce #'(lambda (ls el) (ins2list el ls)) lst ())
)

(defun redmap (fun lst)
    (rreduce #'(lambda (x res) (cons (funcall fun x) res)) lst ())
)

(defun select (lst pred)
    (rreduce
        #'(lambda (x ls) (if (funcall pred x) (cons x ls) ls))
        lst ()
    )
)
