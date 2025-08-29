(add-to-load-path (string-append (dirname (dirname (current-filename))) "/modules"))
(use-modules (sph sace))

(define example (quote (("the test subject" "is simple" "is short"))))

(display (sace-compile example))
(newline)
