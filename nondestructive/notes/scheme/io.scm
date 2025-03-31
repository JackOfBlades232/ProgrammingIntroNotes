#!/usr/bin/csi -s
(import (chicken condition))
(import (chicken process-context))

; Here returning eof is standard, and it's checked w/ eof-object
(define (do-read port len)
    (let ((c (read-char port)))
        (cond
            ((eof-object? c) #t)
            ((eqv? c #\newline)
                (display len)
                (newline)
                (do-read port 0)
            )
            (else (do-read port (+ 1 len)))
        )
    )
)

; io and error handling is given in the following example

; with-exception-handler accepts what to do for exceptions and what to do init
; io ports are got w/ current-...-port
(with-exception-handler
    (lambda (ex) ; what to do on error
        (write (condition->list ex) (current-error-port)) ; condition === error
        (newline (current-error-port))
        (display "Error while opening file\n" (current-error-port))
        (exit 1) ; without this, we get retrigerred
    )
    (lambda () ; main 'what to do'
        (let ((args (command-line-arguments)))
            (if (null? args)
                (do-read (current-input-port) 0)
                (let ((port (open-input-file (car args))))
                    (do-read port 0)
                    (close-input-port port)
                )
            )
        )
    )
)

; this trashy trash can be avoided, cause we can plug our own c stuff in here.
