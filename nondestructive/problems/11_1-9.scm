#!/usr/bin/csi -s

(define (output task what)
    (display task)
    (display ": ")
    (display what)
    (newline)
)

(define (outputc task what comment)
    (display task)
    (display ": ")
    (display what)
    (display " (")
    (display comment)
    (display ")")
    (newline)
)

; 11_1
(define (range n)
    (define (impl n m)
        (if (< m n) '() (cons n (impl (+ n 1) m)))
    )
    (if (integer? n) (impl 1 n) 'WrongInput)
)

(output "11_1" (range 17))
