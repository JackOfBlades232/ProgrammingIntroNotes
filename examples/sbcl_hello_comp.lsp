(defun main ()
    (princ "Hello, world!")
    (terpri)
)

(sb-ext:save-lisp-and-die "sbcl_hello"
    :executable t
    :toplevel 'main
    :compression t
    :purify t
)
