#!/usr/bin/sbcl --script

(defun main (len)
    (let ((c (read-char t nil nil)))
        (cond
            ((eq c nil) t)
            ((eq c #\Newline) (prin1 len) (terpri) (main 0))
            (t (main (+ 1 len)))
        )
    )
)

(main 0)
