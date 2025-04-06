; 11_13/test/palindrome.scm

(import (chicken platform))

(define (palindrome? str) 
    (define (palindrome?-impl str len)
        (cond
            ((<= len 1) #t)
            ((eqv? (string-ref str 0) (string-ref str (- len 1)))
                (if (<= len 3)
                    #t
                    (palindrome?-impl (substring str 1 (- len 1)) (- len 1))
                )
            )
            (#t #f)
        )
    )
    (palindrome?-impl str (string-length str))
)

(return-to-host)
