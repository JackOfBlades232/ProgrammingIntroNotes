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

; lambdas can capture locavars, and this works by lexical context.
; when we use defvar, we instead use dynamic context (i. e. what the var
; is at callsite), and symbol-value uses dynamic context too.
; However, closures, i. e. lambdas, and literals, and defuns, use lex
; context, and a closure captures it (the one that lived when the func obj
; was made)

; example: 
(defun f2 (func thevar) (funcall func thevar))
(defun sample (thevar)
    (f2 #'(lambda (x) (+ x thevar)) 1000)
)

; Here whether we declare dynamic context use matters on value
; If it is dynamic, thevar becomes 1000 by the time the lambda is called
(defvar thevar)
(pr (sample 7))

; The no-defvar behaviour works cause the lambda has an associated lexical
; context which is fixed on creation. thats a closure basically, produced by #'

; symbol value also reaches into dynamic context
(defun var-values (var)
    (cons var (symbol-value 'var))
)
(setq var "glob")
(pr (var-values "loc"))

; let creates a closure w/ locvars and evaluates to last stmt
(let ((counter 0))
    (defun seq-next () (setq counter (+ 1 counter)))
    (defun seq-reset () (setq counter 0))
    (defun seq-print () (pr counter))
)

; looks like an object, doesn't it?
(seq-next)
(seq-print)
(seq-next)
(seq-print)
(seq-reset)
(seq-print)
