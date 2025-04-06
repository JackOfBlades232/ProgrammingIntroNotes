; 11_13/server/logic.scm

(import (chicken platform))

(define (palindrome? str) 
    (define (palindrome?-impl str len)
        (cond
            ((<= len 1) #t)
            ((eqv? (string-ref str 0) (string-ref str (- len 1)))
                (if (<= len 3)
                    #t
                    (palindrome?-impl (substring str 1 (- len 1)) (- len 2))
                )
            )
            (#t #f)
        )
    )
    (palindrome?-impl str (string-length str))
)

(define (step-session state str) 
    (cond
        ((not (integer? state)) (cons #t "Internal server error occured\n"))
        ((< (string-length str) 2) (cons state "String is too short\n"))
        ((palindrome? str)
            (if (>= state 4)
                (cons #t "You have completed the game\n")
                (let ((new-state (+ state 1)))
                    (cons
                        new-state
                        (string-append
                            "A palindrome! "
                            (number->string (- 5 new-state))
                            " to go\n"
                        )
                    )
                )
            )
        )
        (#t (cons state "Not a palindrome\n"))
    )
)

(return-to-host)
