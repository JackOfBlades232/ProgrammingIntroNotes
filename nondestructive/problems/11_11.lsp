#!/usr/bin/sbcl --script

; 2_19 g
(defun process-stdin (balance failed)
    (let ((c (read-char *standard-input* nil nil)))
        (cond
            ((null c) (and (= balance 0) (not failed)))
            ((eql c #\() (process-stdin (+ balance 1) failed))
            ((eql c #\))
                (process-stdin (- balance 1) (or (<= balance 0) failed)))
            (t (process-stdin balance failed))
        )
    )
)
(defun main ()
    (if (process-stdin 0 nil) (princ "YES") (princ "NO"))
    (terpri)
)

(main)
