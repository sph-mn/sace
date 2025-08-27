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
  (let loop ((rest (lines text)) (out (quote ())))
    (if (null? rest) (reverse out)
      (let*
        ( (raw (car rest)) (n (count-prefix-spaces raw)) (d (quotient (+ n 1) 2))
          (t (string-trim-both (string-drop raw n))))
        (loop (cdr rest) (if (string-null? t) out (cons (cons d t) out)))))))

(define (pairs->forest pairs depth)
  (let loop ((rest pairs) (out (quote ())))
    (cond
      ((or (null? rest) (< (caar rest) depth)) (list (reverse out) rest))
      ((> (caar rest) depth) (list))
      (else
        (let*
          ( (txt (cdar rest)) (child-res (pairs->forest (cdr rest) (+ depth 1)))
            (kids (car child-res)) (rest2 (cadr child-res))
            (node (if (null? kids) txt (cons txt kids))))
          (loop rest2 (cons node out)))))))

(define (parse-indent-tree text) (car (pairs->forest (read-indent-pairs text) 0)))
