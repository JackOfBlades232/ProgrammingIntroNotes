#!/usr/bin/sbcl --script
; gcl has definitively dropped the ball -- doesn't have handler-case

; works like this - first is try, next is catch on expression 'types'
; these types are in CLOS -- an OO subsystem of common lisp. Why??

(defvar *ex*)

; now we can not die on permission errors
(handler-case
    (open "/etc/shadow")
    (file-error (err) (setq *ex* err))
)

; but how do we extract info from *ex*?
(princ *ex*)
(terpri)
(prin1 *ex*)
(terpri)

; princ seems to format it, but what if we want to do the message ourselves?

; let's try to redo the line counting for files
; file name will come from args. Different interpreters do it differently,
; in sbcl it's a list *posix-argv*

(defun strlens (file len)
    (let ((c (read-char file nil 'eof)))
        (cond
            ((eq c 'eof) t)
            ((eq c #\Newline) (prin1 len) (terpri) (strlens file 0))
            (t (strlens file (+ 1 len)))
        )
    )
)

(defun process-stdin ()
    (strlens *standard-input* 0)
)

; since this thing does exceptions with 'stack unwinding' style, we also
; want to use the raii style file opening. Not this function may 'throw',
; but will close the file.
(defun process-file (fname)
    (with-open-file
        (f fname :direction :input :if-does-not-exist nil)
        (if (null f)
            (progn
                (princ "Couldn't open file" *error-output*)
                (terpri *error-output*)
                nil
            )
            (strlens f 0)
        )
    )
)

; now, the main program. if no args -- stdin, else file. Also, here we can
; deal with ^C. However, this is interpreter specific.
(handler-case
    (if (cdr *posix-argv*) ; not only script name
        (process-file (second *posix-argv*))
        (process-stdin)
    )
    (file-error (err)
        (princ err *error-output*)
        (terpri *error-output*)
    )
    (sb-sys:interactive-interrupt () nil)
)
