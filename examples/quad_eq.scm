(define (quad-eq a b c throw)
    (if (= a 0) (throw 'NotAQuadraticEq))
    (let ((det (- (* b b) (* 4 a c))))
        (if (< det 0) (throw 'NoRealRoots))
        (let
            (
                (mb2a (/ (- b) (* 2 a)))
                (dq2a (/ (sqrt det) (* 2 a)))
            )
            (list (- mb2a dq2a) (+ mb2a dq2a))
        )
    )
)
