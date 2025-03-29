#!/usr/bin/sbcl --script


(defun princln (what)
    (princ what)
    (terpri)
)

(defun empty (str)
    (eql (length str) 0)
)

(defun noerror (retval)
    (and (listp retval) (not (null retval)))
)

(defun is-ws (c)
    (or (eql c #\Space) (eql c #\Tab))
)

(defun is-digit (c)
    (or
        (eql c #\0) (eql c #\1) (eql c #\2) (eql c #\3) (eql c #\4)
        (eql c #\5) (eql c #\6) (eql c #\7) (eql c #\8) (eql c #\9)
    )
)

(defun digit-to-num (d) (- (char-code d) (char-code #\0)))

(defun eat-ws (line)
    (cond
        ((empty line) line)
        ((is-ws (char line 0)) (eat-ws (subseq line 1)))
        (t line)
    )
)

(defun calculate-value-impl (e accum)
    (cond
        ((empty e) (list accum e))
        ((not (is-digit (char e 0))) (list accum e))
        (t
            (calculate-value-impl
                (subseq e 1)
                (+ (* 10 accum) (digit-to-num (char e 0)))
            )
        )
    )
)
(defun calculate-value (e)
    (let ((se (eat-ws e)))
        (cond
            ((empty se) nil)
            ((eql (char se 0) #\+) (calculate-value (subseq se 1)))
            ((eql (char se 0) #\-)
                (let ((retleft (calculate-value (subseq se 1))))
                    (if (noerror retleft)
                        (let ((ret (car retleft)) (ce (car (cdr retleft))))
                            (list (- ret) ce))
                        retleft
                    )
                )
            )
            ((eql (char se 0) #\()
                (let ((retleft (calculate (subseq se 1))))
                    (if (noerror retleft)
                        (let
                            (
                                (ret (car retleft))
                                (ce (eat-ws (car (cdr retleft))))
                            )
                            (if (and (not (empty ce)) (eql (char ce 0) #\)))
                                (list ret (subseq ce 1))
                                nil
                            )
                        )
                        retleft
                    )
                )
            )
            ((not (is-digit (char se 0))) nil)
            (t (calculate-value-impl se 0))
        )
    )
)

(defun calculate-md-impl (e accum)
    (let ((se (eat-ws e)))
        (cond
            ((empty se) (list accum se))
            ((eql (char se 0) #\*)
                (let ((retleft (calculate-value (subseq se 1))))
                    (if (noerror retleft)
                        (let ((ret (car retleft)) (ce (car (cdr retleft))))
                            (calculate-md-impl ce (* accum ret)))
                        retleft
                    )
                )    
            )
            ((eql (char se 0) #\/)
                (let ((retleft (calculate-value (subseq se 1))))
                    (if (noerror retleft)
                        (let ((ret (car retleft)) (ce (car (cdr retleft))))
                            (if (= ret 0)
                                'zerodiv
                                (calculate-md-impl ce (/ accum ret))
                            )
                        )
                        retleft
                    )
                )    
            )
            ((eql (char se 0) #\%)
                (let ((retleft (calculate-value (subseq se 1))))
                    (if (noerror retleft)
                        (let ((ret (car retleft)) (ce (car (cdr retleft))))
                            (if (= ret 0)
                                'zerodiv
                                (calculate-md-impl ce (mod accum ret))
                            )
                        )
                        retleft
                    )
                )    
            )
            (t (list accum se))
        )
    )
)

(defun calculate-md (e)
    (let ((retleft (calculate-value e)))
        (if (noerror retleft)
            (calculate-md-impl (car (cdr retleft)) (car retleft))
            retleft
        )
    )
)

(defun calculate-impl (e accum)
    (let ((se (eat-ws e)))
        (cond
            ((empty se) (list accum se))
            ((eql (char se 0) #\+)
                (let ((retleft (calculate-md (subseq se 1))))
                    (if (noerror retleft)
                        (let ((ret (car retleft)) (ce (car (cdr retleft))))
                            (calculate-impl ce (+ accum ret)))
                        retleft
                    )
                )    
            )
            ((eql (char se 0) #\-)
                (let ((retleft (calculate-md (subseq se 1))))
                    (if (noerror retleft)
                        (let ((ret (car retleft)) (ce (car (cdr retleft))))
                            (calculate-impl ce (- accum ret)))
                        retleft
                    )
                )    
            )
            (t (list accum se))
        )
    )
)

(defun calculate (e)
    (let ((retleft (calculate-md e)))
        (if (noerror retleft)
            (calculate-impl (car (cdr retleft)) (car retleft))
            retleft
        )
    )
)

(defun main (args)
    (cond
        ((null args) nil)
        ((cdr args) nil)
        (t 
            (let ((retleft (calculate (car args))))
                (cond
                    ((null retleft) (princln "Invalid expression"))
                    ((eq retleft 'zerodiv) (princln "Division by zero"))
                    ((empty (eat-ws (car (cdr retleft))))
                        (princln (car retleft)))
                    (t (princln "Trailing characters"))
                )
            )
        )
    )
)
(main (cdr *posix-argv*))
