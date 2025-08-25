(define-module (sph sace) #:export
  (sace-compile-text sace-compile-list sace-compile) #:use-module
  (sph indent-tree) #:use-module (sph))

(define (fold-left f z xs) (if (null? xs) z (fold-left f (f z (car xs)) (cdr xs))))

(define (words s)
  (let loop ((i 0) (k 0) (out (quote ())))
    (define (push w o) (if (string-null? w) o (cons w o)))
    (if (>= i (string-length s)) (reverse (push (substring s k i) out))
      (if (char=? (string-ref s i) #\space) (loop (+ i 1) (+ i 1) (push (substring s k i) out))
        (loop (+ i 1) k out)))))

(define (spaces a b)
  (if (or (string-null? a) (string-null? b)) (string-append a b) (string-append a " " b)))

(define (ensure-dot s) (if (string-suffix? "." s) s (string-append s ".")))
(define (strip-dot s) (if (string-suffix? "." s) (string-drop-right s 1) s))

(define (coord op xs)
  (cond
    ((null? xs) "")
    ((null? (cdr xs)) (car xs))
    (else (string-join xs (string-append " " op " ")))))

(define (fragment-fold a xs) (fold-left spaces a xs))
(define op-words (quote ("and" "or" "either" "any of" "all of" "not" "if" "then" "else")))
(define prep-words (quote ("from" "to" "with" "by" "for" "within" "of" "at" "before" "after")))
(define (is-op w) (member w op-words))
(define (is-prep w) (member w prep-words))

(define (take-heads ws)
  (let loop ((xs ws) (hs (quote ())))
    (cond
      ((null? xs) (list (reverse hs) (quote ())))
      ( (and (pair? (cdr xs)) (string=? (car xs) "can") (string=? (cadr xs) "be"))
        (loop (cddr xs) (cons "can be" hs)))
      ( (and (pair? (cdr xs)) (string=? (car xs) "all") (string=? (cadr xs) "of"))
        (loop (cddr xs) (cons "all of" hs)))
      ( (and (pair? (cdr xs)) (string=? (car xs) "any") (string=? (cadr xs) "of"))
        (loop (cddr xs) (cons "any of" hs)))
      ( (or (is-op (car xs)) (is-prep (car xs)) (and (null? hs) (not (is-prep (car xs)))))
        (loop (cdr xs) (cons (car xs) hs)))
      (else (list (reverse hs) xs)))))

(define (heads->chain heads tail-kids)
  (let loop ((hs heads) (kids tail-kids))
    (if (null? (cdr hs)) (cons (car hs) kids) (list (car hs) (loop (cdr hs) kids)))))

(define (parse-item-line s)
  (let* ((res (take-heads (words s))) (hs (car res)) (rest (cadr res)))
    (heads->chain hs (if (null? rest) (quote ()) (list (string-join rest " "))))))

(define (attach-children node kids)
  (let ((h (car node)) (cs (cdr node)))
    (if (and (= (length cs) 1) (pair? (car cs)) (string? (caar cs)))
      (list h (attach-children (car cs) kids)) (append (list h) cs kids))))

(define (parse-items raw-items)
  (map
    (lambda (n)
      (let* ((line (car n)) (kids (cadr n)) (node (parse-item-line line)) (sub (parse-items kids)))
        (attach-children node sub)))
    raw-items))

(define (forest->blocks forest) (map (lambda (n) (cons (car n) (parse-items (cadr n)))) forest))
(define (sace-parse text) (forest->blocks (parse-indent-tree text)))
(define (all p xs) (if (null? xs) #t (and (p (car xs)) (all p (cdr xs)))))

(define (compile-frag node ctx)
  (cond
    ((string? node) (strip-dot (spaces ctx node)))
    ((null? node) "")
    (else
      (let*
        ( (h (car node)) (kids (cdr node)) (ys (map (lambda (k) (compile-frag k "")) kids))
          (n (length ys)))
        (cond
          ((string=? h "and") (if (= n 1) (string-append "and " (car ys)) (coord "and" ys)))
          ((string=? h "or") (if (= n 1) (string-append "or " (car ys)) (coord "or" ys)))
          ((string=? h "either") (string-append "either " (if (= n 1) (car ys) (coord "or" ys))))
          ((string=? h "all of") (if (= n 1) (car ys) (coord "and" ys)))
          ((string=? h "any of") (if (= n 1) (car ys) (coord "or" ys)))
          ((string=? h "not") (string-append "not " (coord "and" ys)))
          (else (let ((ctx2 (spaces ctx h))) (if (null? kids) ctx2 (fold-left spaces ctx2 ys)))))))))

(define (partition-ifthen items)
  (let loop ((rest items) (pre (quote ())) (th (quote ())) (el (quote ())) (mode (quote cond)))
    (if (null? rest) (list (reverse pre) (reverse th) (reverse el))
      (let* ((x (car rest)) (h (and (pair? x) (string? (car x)) (car x))))
        (cond
          ((and h (string=? h "then")) (loop (cdr rest) pre (append th (cdr x)) el (quote then)))
          ((and h (string=? h "else")) (loop (cdr rest) pre th (append el (cdr x)) (quote else)))
          ((eq? mode (quote cond)) (loop (cdr rest) (cons x pre) th el mode))
          ((eq? mode (quote then)) (loop (cdr rest) pre (cons x th) el mode))
          (else (loop (cdr rest) pre th (cons x el) mode)))))))

(define (compile-cond items ctx)
  (let*
    ( (res (partition-ifthen items)) (conds (car res)) (th (cadr res))
      (els (caddr res)) (c (coord "or" (map (lambda (it) (compile-frag it "")) conds)))
      (t (if (null? th) ctx (fragment-fold ctx (map (lambda (it) (compile-frag it "")) th))))
      (e (and (not (null? els)) (fragment-fold ctx (map (lambda (it) (compile-frag it "")) els)))))
    (list
      (ensure-dot
        (if e (string-append "if " c " then " t " else " e) (string-append "if " c " then " t))))))

(define (label-context ctx h kids)
  (let ((h1 (and (pair? kids) (pair? (car kids)) (car (car kids)))))
    (if
      (and (string? h) (not (is-op h))
        (not (is-prep h)) (string? h1) (or (string=? h1 "is") (string=? h1 "can be")))
      (string-append "the " h " of " ctx) (spaces ctx h))))

(define (fragmentable? node)
  (cond
    ((string? node) #t)
    ((null? node) #t)
    (else (let ((h (car node))) (not (and (string? h) (string=? h "if")))))))

(define (compile-node node ctx)
  (cond
    ((string? node) (list (ensure-dot (spaces ctx node))))
    ((null? node) (quote ()))
    (else
      (let* ((h (car node)) (kids (cdr node)))
        (cond
          ((and (string? h) (string=? h "if")) (compile-cond kids ctx))
          ( (and (not (null? kids)) (all fragmentable? kids))
            (let ((ctx2 (label-context ctx h kids)))
              (list (ensure-dot (fragment-fold ctx2 (map (lambda (k) (compile-frag k "")) kids))))))
          ((null? kids) (list (ensure-dot (spaces ctx h))))
          (else
            (let ((ctx2 (label-context ctx h kids)))
              (apply append (map (lambda (k) (compile-node k ctx2)) kids)))))))))

(define (compile-block blk)
  (let ((subj (car blk)) (items (cdr blk)))
    (if (null? items) (if (string-suffix? "." subj) (list subj) (quote ()))
      (apply append (map (lambda (it) (compile-node it subj)) items)))))

(define (normalize x)
  (cond
    ((symbol? x) (symbol->string x))
    ((pair? x) (cons (normalize (car x)) (normalize (cdr x))))
    (else x)))

(define (sace-compile a) "(block ...) -> string"
  (apply string-append (apply append (map compile-block (normalize a)))))

(define (sace-compile-text a) "string -> string" (sace-compile (sace-parse a)))
