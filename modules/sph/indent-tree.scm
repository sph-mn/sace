(define-module (sph indent-tree) #:export (parse-indent-tree read-indent-pairs))

(define (lines s)
  (let loop ((i 0) (k 0) (out (quote ())))
    (if (>= i (string-length s)) (reverse (cons (substring s k i) out))
      (if (char=? (string-ref s i) #\newline) (loop (+ i 1) (+ i 1) (cons (substring s k i) out))
        (loop (+ i 1) k out)))))

(define (count-prefix-spaces s)
  (let loop ((i 0))
    (if (and (< i (string-length s)) (char=? (string-ref s i) #\space)) (loop (+ i 1)) i)))

(define (read-indent-pairs text)
  (let loop ((rest (lines text)) (acc (quote ())))
    (if (null? rest) (reverse acc)
      (let*
        ( (raw (car rest)) (n (count-prefix-spaces raw)) (d (quotient (+ n 1) 2))
          (t (string-trim-both (string-drop raw n))))
        (loop (cdr rest) (if (string-null? t) acc (cons (cons d t) acc)))))))

(define (pairs->forest pairs depth)
  (let loop ((rest pairs) (acc (quote ())))
    (cond
      ((or (null? rest) (< (caar rest) depth)) (list (reverse acc) rest))
      ((> (caar rest) depth) (error "invalid indentation" (caar rest) (quote at-depth) depth))
      (else
        (let*
          ( (txt (cdar rest)) (child-res (pairs->forest (cdr rest) (+ depth 1)))
            (kids (car child-res)) (rest2 (cadr child-res)))
          (loop rest2 (cons (list txt kids) acc)))))))

(define (parse-indent-tree text)
  (let* ((res (pairs->forest (read-indent-pairs text) 0))) (car res)))

(define (any->string x)
  (cond
    ((string? x) x)
    ((symbol? x) (symbol->string x))
    ((number? x) (number->string x))
    ((char? x) (string x))
    (else (call-with-output-string (lambda (p) (display x p))))))

(define (list->indent-tree forest)
  (let ((indent (lambda (d) (make-string (* 2 d) #\space))))
    (define (emit node depth)
      (cond
        ((null? node) "")
        ( (pair? node)
          (let* ((head (any->string (car node))) (children (cdr node)))
            (string-append (indent depth) head
              "\n" (apply string-append (map (lambda (child) (emit child (+ depth 1))) children)))))
        (else (string-append (indent depth) (any->string node) "\n"))))
    (apply string-append (map (lambda (node) (emit node 0)) forest))))
