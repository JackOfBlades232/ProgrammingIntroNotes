#!/usr/bin/csi -s
; uncomment for Chicken 5.* (and later?)
; (import (chicken process-context))
; (import (chicken condition))

(define (do_it port len)
    (let ((c (read-char port)))
        (cond
            ((eof-object? c) #t)
            ((eqv? c #\newline) (display len) (newline) (do_it port 0))
            (else (do_it port (+ 1 len)))
        )
    )
)

(with-exception-handler
    (lambda (ex)
        (write (condition->list ex) (current-error-port))
        (newline (current-error-port))
        (display "something wrong (failed to open the file?)\n"
                 (current-error-port))
        (exit 1) ; without this, endless loop occurs
    )
    (lambda ()
        (let ((args (command-line-arguments)))
            (if (null? args)
                (do_it (current-input-port) 0)
                (let ((port (open-input-file (car args))))
                     (do_it port 0)
                     (close-input-port port)
                )
            )
        )
    )
)

