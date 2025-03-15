#!/usr/bin/gcl -f

(defun pr (var)
    (princ var)
    (terpri)
)

; eval calculates it's arg. So we can construct code at runtime and change
; the program under our feet!
(pr (eval (list 'cdr '(list 1 2 3))))

; with intern (creates symbols if they didn't exist) and read (reads S-exprs
; from stdin and 'interns' symbols) there can be a lot of metaprogramming

(pr (function cons)) ; gets the func obj, not from symbol, so no need for '
(pr #'pr) ; same

; call like this. apply takes arg list, funcall -- just args
(apply #'pr '(1));
(funcall #'pr #'pr);

(pr (apply #'+ '(1 2 3 4 5)))

; some other stuff with func objects
(pr (mapcar #'+ '(1 10 100) '(10 100 1000) '(100 1000 10000)))
(pr (mapcar #'list '(1 10 100) '(10 100 1000) '(100 1000 10000)))

; and we can do lambdas, and it's ok to put as first pair elem
(pr ((lambda (a b c) (+ a (* b c))) 10 3 5))
(mapcar #'(lambda (a b c) (pr (list a b c))) '(1 10 100) '(10 100 1000) '(100 1000 10000))

; lambdas can capture locavars
(defun make-adder (n)
    #'(lambda (x) (+ n x))
)

(setq plus13 (make-adder 13))
(pr (funcall plus13 77))

