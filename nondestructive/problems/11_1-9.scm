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

; 11_2
(define (take-first n . expr)
    (define (impl n expr)
        (cond
            ((not (integer? n)) 'WrongInput)
            ((<= n 0) '())
            ((null? expr) '())
            (#t (cons (car expr) (impl (- n 1) (cdr expr))))
        )
    )
    (impl n expr)
)

(output "11_2" (take-first  8 '(1 2 3) "asd" + '(aaa) '() #t '() '() '() '() '()))

; 11_3 -- 11_7 won't redo, it's literally the same

; 11_8
(define (list-iter lst)
    (define state lst)
    (if (list? lst)
        (lambda ()
            (if (null? state)
                '()
                (let ((cur (car state)))
                    (set! state (cdr state))
                    cur
                )
            )
        )
        'WrongInput
    )
)

(define li (list-iter '(1 2 "a")))
(output "11_8" (li))
(output "11_8" (li))
(output "11_8" (li))
(output "11_8" (li))
(output "11_8" (li))
(output "11_8" (list-iter 1))

; 11_9
(define (rev-list lst)
    (define ret '())
    (map (lambda (elem) (set! ret (cons elem ret))) lst)
    ret
)

(output "11_9" (rev-list '(1 2 3 4 5 6 7 8 (9 9))))

; 11_10 is also not getting rewritten in scheme
