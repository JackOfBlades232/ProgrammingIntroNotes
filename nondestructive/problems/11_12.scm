#!/usr/bin/csi -s

(import (chicken process-context))

(define (displn what)
    (display what)
    (newline)
)

(define (empty? str) (= (string-length str) 0))

(define (is-ws? c) (or (eqv? c #\space) (eqv? c #\tab)))

(define (is-digit? c)
    (or
        (eqv? c #\0) (eqv? c #\1) (eqv? c #\2) (eqv? c #\3) (eqv? c #\4)
        (eqv? c #\5) (eqv? c #\6) (eqv? c #\7) (eqv? c #\8) (eqv? c #\9)
    )
)

(define (digit->integer d) (- (char->integer d) (char->integer #\0)))

(define (eat-one-ch line) (substring line 1 (string-length line)))

(define (eat-ws line)
    (cond
        ((empty? line) line)
        ((is-ws? (string-ref line 0)) (eat-ws (eat-one-ch line)))
        (#t line)
    )
)

(define (calculate-value e accum)
    (cond
        ((empty? e) (cons accum e))
        ((not (is-digit? (string-ref e 0))) (cons accum e))
        (#t
            (calculate-value
                (eat-one-ch e)
                (+ (* 10 accum) (digit->integer (string-ref e 0)))
            )
        )
    )
)

(define (calculate-number e throw)
    (let ((se (eat-ws e)))
        (cond
            ((empty? se) (throw 'ExpectingNumber))
            ((eqv? (string-ref se 0) #\+)
                (calculate-number (eat-one-ch se) throw))
            ((eqv? (string-ref se 0) #\-)
                (let ((resrem (calculate-number (eat-one-ch se) throw)))
                    (cons (- (car resrem)) (cdr resrem))
                )
            )
            ((eqv? (string-ref se 0) #\()
                (let ((resrem (calculate (eat-one-ch se) throw)))
                    (let ((res (car resrem)) (ce (eat-ws (cdr resrem))))
                        (if (and (not (empty? ce)) (eqv? (string-ref ce 0) #\)))
                            (cons res (eat-one-ch ce))
                            (throw 'UnmatchedParen)
                        )
                    )
                )
            )
            ((not (is-digit? (string-ref se 0))) (throw 'InvalidChar))
            (#t (calculate-value se 0))
        )
    )
)

(define (calculate-md-tail e accum throw)
    (let ((se (eat-ws e)))
        (cond
            ((empty? se) (cons accum se))
            ((eqv? (string-ref se 0) #\*)
                (let ((resrem (calculate-number (eat-one-ch se) throw)))
                    (let ((res (car resrem)) (ce (cdr resrem)))
                        (calculate-md-tail ce (* accum res) throw))
                )    
            )
            ((eqv? (string-ref se 0) #\/)
                (let ((resrem (calculate-number (eat-one-ch se) throw)))
                    (let ((res (car resrem)) (ce (cdr resrem)))
                        (if (= res 0)
                            (throw 'DivisionByZero)
                            (calculate-md-tail ce (/ accum res) throw)
                        )
                    )
                )    
            )
            ((eqv? (string-ref se 0) #\%)
                (let ((resrem (calculate-number (eat-one-ch se) throw)))
                    (let ((res (car resrem)) (ce (cdr resrem)))
                        (if (= res 0)
                            (throw 'DivisionByZero)
                            (calculate-md-tail ce (modulo accum res) throw)
                        )
                    )
                )    
            )
            (#t (cons accum se))
        )
    )
)

(define (calculate-md e throw)
    (let ((resrem (calculate-number e throw)))
        (calculate-md-tail (cdr resrem) (car resrem) throw))
)

(define (calculate-tail e accum throw)
    (let ((se (eat-ws e)))
        (cond
            ((empty? se) (cons accum se))
            ((eqv? (string-ref se 0) #\+)
                (let ((resrem (calculate-md (eat-one-ch se) throw)))
                    (let ((res (car resrem)) (ce (cdr resrem)))
                        (calculate-tail ce (+ accum res) throw))
                )    
            )
            ((eqv? (string-ref se 0) #\-)
                (let ((resrem (calculate-md (eat-one-ch se) throw)))
                    (let ((res (car resrem)) (ce (cdr resrem)))
                        (calculate-tail ce (- accum res) throw))
                )    
            )
            (#t (cons accum se))
        )
    )
)

(define (calculate e throw)
    (let ((resrem (calculate-md e throw)))
        (calculate-tail (cdr resrem) (car resrem) throw)
    )
)

(define (main args)
    (cond
        ((null? args) (displn "Invalid args"))
        ((not (null? (cdr args))) (displn "Invalid args"))
        (#t 
            (let
                (
                    (caught
                        (call/cc (lambda (throw) (calculate (car args) throw)))
                    )
                )
                (cond
                    ((eq? caught 'UnmatchedParen) (displn "Unmatched paren"))
                    ((eq? caught 'ExpectingNumber) (displn "Expecting number"))
                    ((eq? caught 'InvalidChar) (displn "Invalid digit symbol"))
                    ((eq? caught 'DivisionByZero) (displn "Division by zero"))
                    ((empty? (eat-ws (cdr caught))) (displn (car caught)))
                    (#t (displn "Trailing characters"))
                )
            )
        )
    )
)
(main (command-line-arguments))
