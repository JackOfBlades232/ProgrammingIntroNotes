#!/usr/bin/csi -ss

(define (println line sink)
    (display line sink)
    (newline sink)
)

(define (eqlen-string-eq? a b l i)
    (cond
        ((= (- l i) 0) #t)
        ((eq? (string-ref a i) (string-ref b i))
            (eqlen-string-eq? a b l (+ i 1)))
        (#t #f)
    )
)

(define (string-contains? h n)
    (let ((hl (string-length h)) (nl (string-length n)))
        (cond
            ((= nl 0) #t)
            ((< hl nl) #f)
            ((eqlen-string-eq? (substring h 0 nl) n nl 0) #t)
            (#t (string-contains? (substring h 1 hl) n))
        )
    )
)

(define (process-line needles line)
    (cond
        ((null? needles) #t)
        ((string-contains? line (car needles))
            (println line (current-output-port)))
        (#t (process-line (cdr needles) line))
    )
)

(define (process-stdin needles cur-line)
    (let ((c (read-char)))
        (cond
            ((eof-object? c) (process-line needles cur-line))
            ((eqv? c #\newline)
                (process-line needles cur-line)
                (process-stdin needles "")
            )
            (#t
                (process-stdin needles
                    (string-append cur-line (string c))
                )
            )
        )
    )
)

(define (main args)
    (if args
        (process-stdin args "")
        (and
            (println
                "Invalid args, usage: prog [needles...]"
                (current-error-port)
            )
        )
    )
)
