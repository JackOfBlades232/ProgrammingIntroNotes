#!/usr/bin/gcl -f

; io in (common) lisp is fucked

; print, prin1, princ, pprint
(prin1 1 *standard-output*)
(prin1 1)     ; same
(prin1 1 nil) ; same

(prin1 1 *terminal-io*)  ; should be different, but same in interpreters
(prin1 1 *error-output*) ; should be stderr, but stdout in gcl goddamit

(prin1 1 *debug-io*) ; now this goes to stderr

; also *trace-output*, *query-io*

; all but princ print things in valid S-expr form
(prin1 "asd") ; just prints
(print "asd") ; pprint and then puts a space
(princ "asd") ; prints as the value of the thing (this will print just asd)
(pprint "asd") ; newlines, then prints

(prin1 #\Newline)
(princ #\Newline)

; in truth, print and pprint are hot trash, prin1 is barely useful, leaving princ

(terpri *debug-io*) ; also takes stream names

; format is like fprintf
(format *standard-output* "~d ~s" 4 "ads")
(terpri *standard-output*)
; with nil stream its like sprintf
(setq str (format nil "~d ~s" 4 "ads"))
(princ str)
(terpri)

; for input, there is read to parse out S-exprs, not very useful for user-facing
; so, we read char by char with read-char

; we can read lines, but with some fuckery with getting eofs -- multiple return values
; this binds, see setf
(setf (values line was-eof) (read-line *standard-input* nil nil))
(princ (if was-eof "EOF" line))
(terpri)
; this wraps in a list with consing
(setq ret-lst (multiple-value-list (read-line *standard-input* nil nil)))
(princ ret-lst)
(terpri)

; we'll print the line len in response to every line
; this recursive stuff is fun, not gonna lie
(defun main (len)
    (let ((c (read-char t nil 'eof))) ; this means 1) don't crash on eof 2) return 'eof instead
        (cond
            ((eq c 'eof) t)
            ((eq c #\Newline) (prin1 len) (terpri) (main 0))
            (t (main (+ 1 len)))
        )
    )
)
(main 0)

; this stil has a problem -- stuff like ^C will shoot the thing down and we'll get lisp interp bs.
