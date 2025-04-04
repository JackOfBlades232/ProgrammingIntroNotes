#!/usr/bin/csi -s

; 2_19 d
(define (is-ws c) (or (eqv? c #\space) (eqv? c #\tab) (eqv? c #\newline)))

(define (process-stdin first last)
    (define (good f l) (and (eqv? f #\A) (eqv? l #\z)))
    (let ((c (read-char (current-input-port))))
        (cond
            ((eof-object? c) (if (good first last) 1 0))
            ((is-ws c)
                (if (good first last)
                    (+ 1 (process-stdin c c))
                    (process-stdin c c)
                )
            )
            ((is-ws last) (process-stdin c c))
            (#t (process-stdin first c))
        )
    )
)

(define (main)
    (display (process-stdin #\space #\space))
    (newline)
)

(main)
