#!/usr/bin/sbcl --script
; NOTE: gcl literally could not tail call the read-char loop)))

; here there are keyword params. weird (go in pairs name val)
; again, we need to save ourselves from interpreter panic
(setq inf (open "io.lsp" :if-does-not-exist nil))
; input by default.
(setq outf
    (open
        "log"
        :direction :output
        :if-does-not-exist :create
        :if-exists :overwrite
    )
)

; also, :... are identifier constants, don't get calcd

; however, we will still die on insufficient priviliges, etc

; there are also binary w/ :element-type 'signed-byte/'unsigned byte and
; write-byte/read-byte, will not check it out.

; Now we can use this stuff as streams
(defun main (c)
    (if (null c)
        t
        (let ()
            (princ c outf)
            (let ((nc (read-char inf nil nil))) (main nc))
        )
    )
)
(main (read-char inf nil))

(close inf)
(close outf)
