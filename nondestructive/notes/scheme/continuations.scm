#!/usr/bin/csi -s

; a continuation is 'the rest of the program after this function returns'
; if you think of it, it's a concrete callable thing that takes one arg
; (the functions retval). At each point in the program there is exactly one
; continuation, and scheme allows calling it explicitly with an arg.
; It's actually a first class object, furthermore.
;
; NOTE: it also exists in something like C -- represented by the rest of the
; stack and all return addresses. Of course, it can't be called at will cause
; it can modify global state (for example, calling a continuation twice in c++
; with raii pointers will double free them). Although you could say that 
; throwing an exception is like calling the continuation of the try block.
;
; Also this notion can prove useful for concurrency (see Roman Lipovsky's stuff
; on gitlab)

; in scheme the only way to get the continuation is this
(display (call/cc (lambda (cc) (cc 25))))

; this thing calls the function that is given to it with the argument of
; it's own continuation. In the above example the lambda never returns -- 
; instead we call the continuation of this call/cc invokation passing 25 to it.

; if we don't call the given continuation, the whole thing just regularly
; returns the thing

(display (call/cc (lambda (cc) 25)))

; Here are some more adequate uses

; We can make early returns
(define (earlyret-func x)
    (call/cc (lambda (return)
        (if (not (integer? x))
            (return #f))
        (display x)
        (newline)
        #t
    ))
)

(earlyret-func 13)
(earlyret-func #\n)

(define (search wanted? lst)
    (call/cc (lambda (return)
        (for-each ; like map, but for side-effect
            (lambda (element)
                (if (wanted? element) (return element))
            )
            lst
        )
        #f
    ))
)

(display (search number? '('() 2 3)))
(display (search list? '(1 2 3)))
(newline)

; this thing can also break out of nested calls

(define (traverse lst wanted? cb)
    (for-each ; like map, but for side-effect
        (lambda (element) (if (wanted? element) (cb element)))
        lst
    )
)

(define (search2 wanted? lst)
    (call/cc (lambda (return)
        (traverse lst wanted? return)
    ))
)

(display (search2 number? '('() 2 3)))
(display (search2 list? '(1 2 3)))
(newline)

; Now we can easily do exceptions -- just give every functions the continuation
; to 'throw' as a parameter

(define (quad-eq a b c throw)
    (if (= a 0) (throw 'NotAQuadraticEq))
    (let ((discrim  (- (* b b) (* 4 a c))))
        (if (< discrim 0) (throw 'NoRealRoots))
        (let ((mb2a (/ (- b) (* 2 a))) (dq2a (/ (sqrt discrim) (* 2 a))))
            (list (- mb2a dq2a) (+ mb2a dq2a))
        )
    )
)

; now we can do a function that does a try catch (i'd use it for a parser)
(define (quad-eq-user-facing a b c)
    (let ((caught (call/cc (lambda (throw) (quad-eq a b c throw)))))
        (cond
            ((list? caught) (display "Answer: ") (display caught))
            ((eq? caught 'NotAQuadraticEq)
                (display "Malformed quadratic eq.")
            )
            ((eq? caught 'NoRealRoots)
                (display "No roots.")
            )
        )
        (newline)
    )
)

; Now we can call such a function either from a call/cc which is catching,
; or another function that supports throwing
(quad-eq-user-facing 1 -2 1)
(quad-eq-user-facing 1 1 -20)
(quad-eq-user-facing 0 1 -20)
(quad-eq-user-facing 1 1 10)

; Another example -- something like a goto

; this way we can extract the continuation object, like making a snapshot
(define (right-now) (call/cc (lambda (cc) (cc cc))))

; and this way we can go to that snapshot!
(define (go-when then) (then then))


; now let's do a 20'th century hello world
(let ((rest 5))
    (let ((the-beginning (right-now)))
        (display "Hello, world!\n")
        (set! rest (- rest 1))
        (if (> rest 0) (go-when the-beginning))
    )
)


; there is a thing called continuation passing style, where we don't return
; categorically: every function receives a continuation. I'm looking at you,
; functional futures.

; here is an example

; these are operators rewriten with cont-passing style
(define (+& x y k) (k (+ x y)))
(define (*& x y k) (k (* x y)))
(define (sqrt& x k) (k (sqrt x)))

; now we can do pythagorean theorem like this kekw
(define (pyth& x y k)
    (*& x x
        (lambda (x2) (*& y y
            (lambda (y2) (+& x2 y2
                (lambda (x2py2) (sqrt& x2py2 k))
            ))
        ))
    )
)

; looks like shit, but if this were written as pipes, would be quite nice
(pyth& 4 5 display)
(newline)

