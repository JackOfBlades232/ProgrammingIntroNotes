#!/usr/bin/sbcl --script

(defun output (task what)
    (princ task)
    (princ ": ")
    (princ what)
    (terpri)
)

(defun get-longest-arg (args curmax curmaxlen)
    (cond
        ((null args) curmax)
        (t
            (let ((arg (car args)) (arglen (length (car args))))
                (if (> arglen curmaxlen)
                    (get-longest-arg (cdr args) arg arglen)
                    (get-longest-arg (cdr args) curmax curmaxlen)
                )
            )
        )
    )
)

(defun has-char-impl (str pred curid)
    (cond
        ((eql (length str) curid) nil)
        ((funcall pred (char str curid)) t)
        (t (has-char-impl str pred (+ curid 1)))
    )
)

(defun count-char-impl (str pred curid)
    (cond
        ((eql (length str) curid) 0)
        ((funcall pred (char str curid))
            (+ 1 (count-char-impl str pred (+ curid 1))))
        (t (count-char-impl str pred (+ curid 1)))
    )
)

(defun has-conc-char-impl (str cc curid)
    (has-char-impl str #'(lambda (c) (equal c cc)) curid)
)
(defun count-conc-char-impl (str cc curid)
    (count-char-impl str #'(lambda (c) (equal c cc)) curid)
)

(defun all-chars-impl (str pred curid)
    (not (has-char-impl str #'(lambda (c) (not (funcall pred c))) curid))
)
(defun all-chars-conc-impl (str cc curid)
    (all-chars-impl str #'(lambda (c) (equal c cc)) curid)
)

(defun has-char (str pred) (has-char-impl str pred 0))
(defun count-char (str pred) (count-char-impl str pred 0))
(defun all-chars (str pred) (all-chars-impl str pred 0))
(defun has-conc-char (str cc) (has-conc-char-impl str cc 0))
(defun count-conc-char (str cc) (count-conc-char-impl str cc 0))
(defun all-chars-conc (str cc) (all-chars-conc-impl str cc 0))

(defun is-digit (c)
    (or
        (eql c #\0) (eql c #\1) (eql c #\2) (eql c #\3) (eql c #\4)
        (eql c #\5) (eql c #\6) (eql c #\7) (eql c #\8) (eql c #\9)
    )
)

(defun is-letter (c)
    (or
        (and (char>= c #\A) (char<= c #\Z))
        (and (char>= c #\a) (char<= c #\z))
    )
)

(defun is-ws (c)
    (or (eql c #\Space) (eql c #\Tab))
)

(defun has-rep-chars-dumb-impl (str curid)
    (cond
        ((eql (length str) curid) nil)
        ((has-conc-char-impl str (char str curid) (+ curid 1)) t)
        (t (has-rep-chars-dumb-impl str (+ curid 1)))
    )
)
(defun has-rep-chars-dumb (str) (has-rep-chars-dumb-impl str 0))

(defun is-same-char (str)
    (cond
        ((<= (length str) 1) t)
        (t (all-chars-conc-impl str (char str 0) 1))
    )
)

(defun have-common-chars-dumb-impl (str haystack curid)
    (cond
        ((eql (length str) curid) nil)
        ((has-conc-char haystack (char str curid)) t)
        (t (have-common-chars-dumb-impl str haystack (+ curid 1)))
    )
)
(defun have-common-chars-dumb (str haystack)
    (have-common-chars-dumb-impl str haystack 0)
)

(defun token-cnt-impl (str curid)
    (cond
        ((eql (length str) curid) (if (is-ws (char str (- curid 1))) 0 1))
        (t
            (+
                (if
                    (or
                        (is-ws (char str (- curid 1)))
                        (not (is-ws (char str curid)))
                    )
                    0 1
                )
                (token-cnt-impl str (+ curid 1))
            )
        )
    )
)
(defun token-cnt (str)
    (if (eql (length str) 0) 0 (token-cnt-impl str 1))
)

(defun has-substring-at-impl (str substr strid substrid)
    (cond
        ((eql (length substr) substrid) t)
        ((eql (length str) strid) nil)
        ((eql (char str strid) (char substr substrid))
            (has-substring-at-impl str substr (+ strid 1) (+ substrid 1)))
        (t nil)
    )
)
(defun has-substring-at (str substr strid)
    (has-substring-at-impl str substr strid 0)
)

(defun count-inclusions-impl (str substr strid)
    (cond
        ((< (- (length str) strid) (length substr)) 0)
        (t
            (+
                (if (has-substring-at str substr strid) 1 0)
                (count-inclusions-impl str substr (+ strid 1))
            )
        )
    )
)
(defun count-inclusions (str substr) (count-inclusions-impl str substr 0))

; 2_22
(defun process-args-2-22-a (args)
    (let ((longest (get-longest-arg args nil -1)))    
        (princ longest)
        (princ " ")
        (princ (length longest))
        (terpri)
    )
)

(defun process-args-2-22-b (args)
    (cond
        ((null args) nil)
        (t
            (if (not (has-rep-chars-dumb (car args)))
                (progn
                    (princ (car args))
                    (terpri)
                )
            )
            (process-args-2-22-b (cdr args))
        )
    )
)

(defun process-args-2-22-c (args)
    (cond
        ((null args) nil)
        (t
            (if
                (and
                    (has-conc-char (car args) #\.)
                    (= (count-conc-char (car args) #\@) 1)
                )
                (progn
                    (princ (car args))
                    (terpri)
                )
            )
            (process-args-2-22-c (cdr args))
        )
    )
)

(defun process-args-2-22-d (args)
    (cond
        ((null args) nil)
        (t
            (if (all-chars (car args) #'is-digit)
                (progn
                    (princ (car args))
                    (terpri)
                )
            )
            (process-args-2-22-d (cdr args))
        )
    )
)

(defun process-args-2-22-e (args)
    (cond
        ((null args) nil)
        (t
            (if (is-same-char (car args))
                (progn
                    (princ (car args))
                    (terpri)
                )
            )
            (process-args-2-22-e (cdr args))
        )
    )
)

(defun process-args-2-22-f (args)
    (cond
        ((null args) nil)
        (t
            (if (has-char (car args) #'is-letter)
                (progn
                    (princ (car args))
                    (terpri)
                )
            )
            (process-args-2-22-f (cdr args))
        )
    )
)

(defun process-args-2-22-g-impl (str args)
    (cond
        ((null args) nil)
        (t
            (if (have-common-chars-dumb str (car args))
                (progn
                    (princ (car args))
                    (terpri)
                )
            )
            (process-args-2-22-g-impl str (cdr args))
        )
    )
)
(defun process-args-2-22-g (args)
    (cond
        ((null args) nil)
        (t (process-args-2-22-g-impl (car args) (cdr args)))
    )
)

; 2_23
(defun process-args-2-23 (args)
    (cond
        ((null args) nil)
        ((cdr args) nil)
        (t
            (progn
                (princ (token-cnt (car args)))
                (terpri)
            )
        )
    )
)

; 4_12
(defun process-args-4-12-impl (str args cb)
    (cond
        ((null args) nil)
        (t
            (funcall cb (car args) (count-inclusions (car args) str))
            (process-args-4-12-impl str (cdr args) cb)
        )
    )
)
(defun process-args-4-12-a (args)
    (cond
        ((null args) nil)
        (t
            (process-args-4-12-impl
                (car args) (cdr args)
                #'(lambda (arg cnt)
                    (if (> cnt 0)
                        (progn
                            (princ arg)
                            (terpri)
                        )
                    )
                )
            )
        )
    )
)
(defun process-args-4-12-b (args)
    (cond
        ((null args) nil)
        (t
            (process-args-4-12-impl
                (car args) (cdr args)
                #'(lambda (arg cnt)
                    (if (> cnt 0)
                        (progn
                            (princ arg)
                            (princ " ")
                            (princ cnt)
                            (terpri)
                        )
                    )
                )
            )
        )
    )
)

; main
(defun main (args)
    (cond
        ((null args) nil)
        ((equal (car args) "2_22")
            (cond
                ((cdr args)
                    (cond
                        ((equal (car (cdr args)) "a")
                            (process-args-2-22-a (cdr (cdr args)))
                        )
                        ((equal (car (cdr args)) "b")
                            (process-args-2-22-b (cdr (cdr args)))
                        )
                        ((equal (car (cdr args)) "c")
                            (process-args-2-22-c (cdr (cdr args)))
                        )
                        ((equal (car (cdr args)) "d")
                            (process-args-2-22-d (cdr (cdr args)))
                        )
                        ((equal (car (cdr args)) "e")
                            (process-args-2-22-e (cdr (cdr args)))
                        )
                        ((equal (car (cdr args)) "f")
                            (process-args-2-22-f (cdr (cdr args)))
                        )
                        ((equal (car (cdr args)) "g")
                            (process-args-2-22-g (cdr (cdr args)))
                        )
                        (t nil)
                    )
                )
                (t nil)
            )
        )
        ((equal (car args) "2_23") (process-args-2-23 (cdr args)))
        ((equal (car args) "4_12")
            (cond
                ((cdr args)
                    (cond
                        ((equal (car (cdr args)) "a")
                            (process-args-4-12-a (cdr (cdr args)))
                        )
                        ((equal (car (cdr args)) "b")
                            (process-args-4-12-b (cdr (cdr args)))
                        )
                        (t nil)
                    )
                )
                (t nil)
            )
        )
        (t nil)
    )
)
(main (cdr *posix-argv*))
