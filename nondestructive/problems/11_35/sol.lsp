#!/usr/bin/sbcl --script

(defun println (line sink)
    (princ line sink)
    (terpri sink)
)

(defun process-line (needles line)
    (cond
        ((null needles) t)
        ((search (car needles) line) (println line *standard-output*))
        (t (process-line (cdr needles) line))
    )
)

(defun process-stdin (needles cur-line)
    (let ((c (read-char t nil 'eof)))
        (cond
            ((eq c 'eof) (process-line needles cur-line))
            ((eq c #\Newline)
                (process-line needles cur-line)
                (process-stdin needles "")
            )
            (t
                (process-stdin needles
                    (concatenate 'string cur-line (string c))
                )
            )
        )
    )
)

(handler-case
    (if (cdr *posix-argv*)
        (process-stdin (cdr *posix-argv*) "")
        (progn
            (princ "Invalid args, usage: prog [needles...]" *error-output*)
            (terpri *error-output*)
        )
    )
    (sb-sys:interactive-interrupt () nil)
)
