#!/usr/bin/sbcl --script

(defun do_it (file len)
    (let ((c (read-char file nil nil)))
        (cond
            ((eq c nil) t)
            ((eq c #\Newline) (prin1 len) (terpri) (do_it file 0))
            (t (do_it file (+ 1 len)))
        )
    )
)

; (defun process_file (fname)
;     (princ fname)
;     (terpri)
;     (with-open-stream
;         (f (open fname :direction :input :if-does-not-exist nil))
;         (if (null f)
;             (progn
;                 (princ "Couldn't open the file" *error-output*)
;                 (terpri *error-output*)
;                 nil
;             )
;             (do_it f 0)
;         )
;     )
; )

(defun process_file (fname)
    (with-open-file (f fname :direction :input :if-does-not-exist nil)
        (if (null f)
            (progn
                (princ "Couldn't open the file" *error-output*)
                (terpri *error-output*)
                nil
            )
            (do_it f 0)
        )
    )
)

(defun process_stdin ()
    (do_it *standard-input* 0)
)

; (let
;     ((lst
;         (multiple-value-list (ignore-errors
;             (if (cdr *posix-argv*)
;                 (process_file (second *posix-argv*))
;                 (process_stdin)
;             )
;             t
;         ))
;     )) ;;
;     (if (not (car lst))
;         (progn
;             (princ "Something went wrong" *error-output*)
;             (terpri *error-output*)
;             (princ (second lst) *error-output*)
;             (terpri *error-output*)
;             (prin1 (second lst) *error-output*)
;             ;(inspect (second lst))
;             ;(terpri *error-output*)
;             (princ
;                 (third (slot-value (second lst) 'sb-kernel::format-arguments))
;                 *error-output*
;             )
;             (terpri *error-output*)
;             (princ "The exception type is: " *error-output*)
;             (princ (type-of (second lst)) *error-output*)
;             (terpri *error-output*)
;             (princ
;                 (if (typep (second lst) 'file-error)
;                     "Yes, it is file error!"
;                     "No, it is not a file error. Strange."
;                 )
;                 *error-output*
;             )
;             (terpri *error-output*)
;             nil
;         )
;     )
; )

(handler-case
    (if (cdr *posix-argv*)
        (process_file (second *posix-argv*))
        (process_stdin)
    )
    ;;
    (file-error (er)
        (princ er *error-output*)
        (terpri *error-output*)
    )
    (sb-sys:interactive-interrupt () nil)
)
