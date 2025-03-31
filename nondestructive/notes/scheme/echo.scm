#!/usr/bin/csi -ss

; an -ss script needs to have a main func and gives it args (w/out prog name)
; if compiled, needs a -postlude '(main command-line-arguments)'

(define (main args)
    (map (lambda (s) (display s) (display " ")) args)
    (newline)
)
