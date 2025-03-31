#!/usr/bin/csi -s

; needed for funcs below for csm 5+
(import (chicken process-context))

; a function, chicken-scheme specific
(display (program-name))
(newline)

; this is also a func, does not include prog name
(display (command-line-arguments))
(newline)

; here are some diffs from lisp
(display "Huh\n") ; c style escape chars work
(display (or #t #f)) ; true end false literals, are like this, false is not nil
(newline)

; destructive funcs w/ !
(define x) ; setting w/out defining is a non-standard ext
(set! x 1) ; also set is not defined to return a val
(display x)
(newline)
; predicate funcs end w/ ?
(display (eq? 1 x))
(newline)
; casts are ->
(display (char->integer #\c))
(newline)
; comparisons are 'explicit'
(display (string>? "asd" "sda"))
(newline)

; incomplete if also may not return a value, if the null branch is taken,
; making it not as useful
(if (> 1 2) (display "Unreachable"))

; The fact that not all exprs return values means funcs can be 'void'


; One of the most important diffs is as follows:
; symbols don't have a special notion of an associated function,
; the function is just the value of a symbol and is computed. This means:
; 1) Funquotes are not needed, or funcall
; 2) For func calls all elements of an expr get computed, including the first.
;    (unless it means a specform or a macro)
; 3) Vars and funcs are defined with the same specform define

(map (lambda (x) (display x) (newline)) '("AA" "BB" "CC"))
; and in define name and args go together in first list
(define (f x) (display x) (newline) (newline))
(define somevar 257)
(f somevar)
(define ff (lambda (x) (display x) (newline) (newline))) ; same

; this first list can be irregular -- this means variadic func. Rest is a list
(define (vf x . rest)
    (display x)
    (if (null? rest) (newline) (vf rest))
)

(vf 1 2 3 4 5 9)

(write "asd\n") ; like prin1 -- prints a a lisp thingy
(newline)

