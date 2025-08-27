(define-module (sph sace) #:export (sace-compile-text sace-compile) #:use-module (sph indent-tree))
(define (fold-left f z xs) (if (null? xs) z (fold-left f (f z (car xs)) (cdr xs))))

(define (spaces a b)
  (if (or (string-null? a) (string-null? b)) (string-append a b) (string-append a " " b)))

(define (ensure-dot s) (if (string-suffix? "." s) s (string-append s ".")))
(define (strip-dot s) (if (string-suffix? "." s) (string-drop-right s 1) s))

(define (words s)
  (let loop ((i 0) (k 0) (out (quote ())))
    (let ((push (lambda (w o) (if (string-null? w) o (cons w o)))))
      (if (>= i (string-length s)) (reverse (push (substring s k i) out))
        (if (char=? (string-ref s i) #\space) (loop (+ i 1) (+ i 1) (push (substring s k i) out))
          (loop (+ i 1) k out))))))

(define (leading-token s)
  (let loop ((i 0) (k 0))
    (cond
      ((>= i (string-length s)) (substring s k i))
      ((char=? (string-ref s i) #\space) (substring s k i))
      (else (loop (+ i 1) k)))))

(define q-wh-subjects (quote ("who" "what" "which" "how" "how many")))
(define (subject-wh-question? subj) (member (leading-token subj) q-wh-subjects))

(define (ensure-end s is-q)
  (if is-q (if (string-suffix? "?" s) s (string-append s "?"))
    (if (string-suffix? "." s) s (string-append s "."))))

(define op-words (quote ("and" "or" "either" "any of" "all of" "not" "if" "then" "else" "iff")))
(define prep-words (quote ("from" "to" "with" "by" "for" "within" "of" "at" "before" "after")))
(define rel-words (quote ("that" "who" "which")))

(define det-words
  (quote ("every" "each" "no" "exactly" "at least" "at most" "more than" "less than")))

(define verbish-words
  (quote ("is" "are" "has" "have" "can" "will" "does" "do" "occurs" "occur" "apply" "applies" "be")))

(define (is-op? w) (member w op-words))
(define (is-prep? w) (member w prep-words))
(define (is-verbish? w) (member w verbish-words))

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
      ( (and (pair? (cdr xs)) (string=? (car xs) "at") (string=? (cadr xs) "least"))
        (loop (cddr xs) (cons "at least" hs)))
      ( (and (pair? (cdr xs)) (string=? (car xs) "at") (string=? (cadr xs) "most"))
        (loop (cddr xs) (cons "at most" hs)))
      ( (and (pair? (cdr xs)) (string=? (car xs) "more") (string=? (cadr xs) "than"))
        (loop (cddr xs) (cons "more than" hs)))
      ( (and (pair? (cdr xs)) (string=? (car xs) "less") (string=? (cadr xs) "than"))
        (loop (cddr xs) (cons "less than" hs)))
      ((or (is-op? (car xs)) (is-prep? (car xs)) (null? hs)) (loop (cdr xs) (cons (car xs) hs)))
      (else (list (reverse hs) xs)))))

(define (parse-item-line s)
  (let*
    ( (ws (words s)) (res (take-heads ws)) (hs (car res))
      (rest (cadr res)) (kids (if (null? rest) (quote ()) (list (string-join rest " ")))))
    (let chain ((hs hs) (kids kids))
      (if (null? (cdr hs)) (cons (car hs) kids) (list (car hs) (chain (cdr hs) kids))))))

(define (attach-children node kids)
  (let ((h (car node)) (cs (cdr node)))
    (if (and (= (length cs) 1) (pair? (car cs)) (string? (caar cs)))
      (list h (attach-children (car cs) kids)) (append (list h) cs kids))))

(define (parse-items nodes)
  (map
    (lambda (n)
      (let*
        ( (line (if (string? n) n (car n))) (kids (if (string? n) (quote ()) (cdr n)))
          (node (parse-item-line line)) (sub (parse-items kids)))
        (attach-children node sub)))
    nodes))

(define (norm-tree n)
  (cond
    ((symbol? n) (symbol->string n))
    ((string? n) n)
    ((pair? n) (cons (norm-tree (car n)) (map norm-tree (cdr n))))
    (else n)))

(define (sace-parse text)
  (map
    (lambda (n)
      (let*
        ( (line (if (string? n) n (car n))) (kids (if (string? n) (quote ()) (cdr n)))
          (chain (parse-item-line line)) (sub (parse-items kids)) (tree (attach-children chain sub)))
        (cons (car tree) (cdr tree))))
    (parse-indent-tree text)))

(define (coord op xs)
  (cond
    ((null? xs) "")
    ((null? (cdr xs)) (car xs))
    (else (string-join xs (string-append " " op " ")))))

(define (fragment-fold a xs) (fold-left spaces a xs))

(define (compile-frag node subj)
  (cond
    ((string? node) (strip-dot (spaces subj node)))
    ((null? node) "")
    (else
      (let* ((h (car node)) (kids (cdr node)) (ys (map (lambda (k) (compile-frag k "")) kids)))
        (cond
          ((string=? h "and") (string-join ys " and "))
          ((string=? h "or") (string-join ys " or "))
          ((string=? h "either") (string-append "either " (string-join ys " or ")))
          ((string=? h "all of") (coord "and" ys))
          ((string=? h "any of") (coord "or" ys))
          ((string=? h "not") (string-append "not " (coord "and" ys)))
          (else (let ((ctx2 (spaces subj h))) (if (null? kids) ctx2 (fold-left spaces ctx2 ys)))))))))

(define (number-string? s) (and (string? s) (string->number s)))

(define (child-head-token k)
  (cond
    ( (string? k)
      (let loop ((i 0) (k0 0))
        (cond
          ((>= i (string-length k)) (substring k k0 i))
          ((char=? (string-ref k i) #\space) (substring k k0 i))
          (else (loop (+ i 1) k0)))))
    ((pair? k) (car k))
    (else "")))

(define (split-op-suffix h)
  (let ((h (string-trim-both h)))
    (cond
      ((string-suffix? " any of" h) (list (string-drop-right h 7) "any of"))
      ((string-suffix? " all of" h) (list (string-drop-right h 7) "all of"))
      ((string-suffix? " either" h) (list (string-drop-right h 7) "either"))
      ((string-suffix? " and" h) (list (string-drop-right h 4) "and"))
      ((string-suffix? " or" h) (list (string-drop-right h 3) "or"))
      (else (list h #f)))))

(define (compile-if-branch items subj)
  (let*
    ( (items
        (let ((x (car items)))
          (if (and (string? x) (string-prefix? "if " x))
            (cons (substring x 3 (string-length x)) (cdr items)) items)))
      (split
        (let loop
          ((xs items) (conds (quote ())) (th (quote ())) (el (quote ())) (mode (quote cond)))
          (if (null? xs) (list (reverse conds) (reverse th) (reverse el))
            (let*
              ( (x (car xs)) (is-then (and (pair? x) (string? (car x)) (string=? (car x) "then")))
                (is-else (and (pair? x) (string? (car x)) (string=? (car x) "else")))
                (sx (and (string? x) x)))
              (cond
                (is-then (loop (cdr xs) conds (append th (cdr x)) el (quote then)))
                (is-else (loop (cdr xs) conds th (append el (cdr x)) (quote else)))
                ( (and sx (string-prefix? "then " sx))
                  (loop (cdr xs) conds (cons (substring sx 5 (string-length sx)) th) el mode))
                ( (and sx (string-prefix? "else " sx))
                  (loop (cdr xs) conds th (cons (substring sx 5 (string-length sx)) el) mode))
                ((eq? mode (quote cond)) (loop (cdr xs) (cons x conds) th el mode))
                ((eq? mode (quote then)) (loop (cdr xs) conds (cons x th) el mode))
                (else (loop (cdr xs) conds th (cons x el) mode)))))))
      (conds0 (car split)) (th (cadr split))
      (el (caddr split))
      (conds
        (apply append
          (map
            (lambda (c)
              (cond
                ((and (pair? c) (string? (car c)) (string=? (car c) "if")) (cdr c))
                ( (and (pair? c) (string? (car c)) (string-prefix? "if " (car c)))
                  (list (cons (substring (car c) 3 (string-length (car c))) (cdr c))))
                ( (and (string? c) (string-prefix? "if " c))
                  (list (substring c 3 (string-length c))))
                (else (list c))))
            conds0)))
      (cond-text (coord "or" (map (lambda (it) (compile-frag it "")) conds)))
      (then-text
        (if (null? th) subj (fold-left spaces subj (map (lambda (it) (compile-frag it "")) th))))
      (else-text
        (and (not (null? el)) (fold-left spaces subj (map (lambda (it) (compile-frag it "")) el)))))
    (list
      (ensure-dot
        (if else-text (string-append "if " cond-text " then " then-text " else " else-text)
          (string-append "if " cond-text " then " then-text))))))

(define (compile-determiner-branch head kids subj)
  (let*
    ( (split
        (let loop ((ks kids) (nums (quote ())) (rels (quote ())) (body (quote ())))
          (if (null? ks) (list (reverse nums) (reverse rels) (reverse body))
            (let* ((k (car ks)) (tok (child-head-token k)))
              (cond
                ((member tok rel-words) (loop (cdr ks) nums (cons (compile-frag k "") rels) body))
                ((number-string? tok) (loop (cdr ks) (cons (compile-frag k "") nums) rels body))
                (else (loop (cdr ks) nums rels (cons k body))))))))
      (nums (list-ref split 0)) (rels (list-ref split 1))
      (body (list-ref split 2)) (det-np (string-join (append (list head) nums (list subj) rels) " "))
      (subj2 det-np))
    (let* ((det-np (string-join (append (list head) nums (list subj) rels) " ")) (subj2 det-np))
      (if (null? body) (list (ensure-dot (string-append "there is " det-np)))
        (apply append (map (lambda (k) (compile-node k subj2 #f)) body))))))

(define (compile-node node subj top?)
  (cond
    ((string? node) (list (ensure-end (spaces subj node) (subject-wh-question? subj))))
    ((null? node) (quote ()))
    (else
      (let* ((h (car node)) (kids (cdr node)))
        (cond
          ((and (string? h) (member h det-words)) (compile-determiner-branch h kids subj))
          ( (and (string? h) (string=? h "iff"))
            (let*
              ( (lhs (compile-frag (car kids) "")) (rhs (compile-frag (cadr kids) ""))
                (sent (spaces subj (string-append lhs " if and only if " rhs))))
              (list (ensure-end sent (subject-wh-question? subj)))))
          ((and (string? h) (string=? h "if")) (compile-if-branch kids subj))
          ( (and (not (null? kids))
              (let frag? ((xs kids))
                (if (null? xs) #t
                  (let ((x (car xs)))
                    (and
                      (or (string? x)
                        (and (pair? x)
                          (let ((hh (car x))) (not (and (string? hh) (string=? hh "if"))))))
                      (frag? (cdr xs)))))))
            (let*
              ( (sp (split-op-suffix h)) (h0 (car sp)) (op (cadr sp))
                (subj2 (spaces subj h0)) (ys (map (lambda (k) (compile-frag k "")) kids))
                (tail
                  (cond
                    ((not op) (fragment-fold "" ys))
                    ((string=? op "and") (coord "and" ys))
                    ((string=? op "or") (coord "or" ys))
                    ((string=? op "all of") (coord "and" ys))
                    ((string=? op "any of") (coord "or" ys))
                    ((string=? op "either") (string-append "either " (coord "or" ys)))
                    (else (fragment-fold "" ys))))
                (sent (spaces subj2 tail)))
              (list (ensure-end sent (subject-wh-question? subj)))))
          ( (null? kids)
            (let ((sent (spaces subj h))) (list (ensure-end sent (subject-wh-question? subj)))))
          (else
            (let ((subj2 (spaces subj h)))
              (apply append (map (lambda (k) (compile-node k subj2 #f)) kids)))))))))

(define (compile-block block)
  (let ((subj (car block)) (items (cdr block)))
    (if (null? items) (list (ensure-end subj (subject-wh-question? subj)))
      (let loop ((xs items) (acc (quote ())))
        (if (null? xs) (apply append (reverse acc))
          (let* ((it (car xs)) (h (and (pair? it) (car it))) (hs (and (string? h) h)))
            (if (and hs (or (string=? hs "if") (string-prefix? "if " hs)))
              (let scan ((ys (cdr xs)) (collected (list it)))
                (if (null? ys) (loop ys (cons (compile-if-branch collected subj) acc))
                  (let*
                    ( (y (car ys)) (yh (and (pair? y) (car y))) (yhs (and (string? yh) yh))
                      (ystr (and (string? y) y)))
                    (if
                      (or (and yhs (or (string=? yhs "then") (string=? yhs "else")))
                        (and ystr (or (string-prefix? "then " ystr) (string-prefix? "else " ystr))))
                      (scan (cdr ys) (append collected (list y)))
                      (loop ys (cons (compile-if-branch collected subj) acc))))))
              (loop (cdr xs) (cons (compile-node it subj #t) acc)))))))))

(define (normalize t)
  (cond
    ((symbol? t) (symbol->string t))
    ((number? t) (number->string t))
    ((pair? t) (cons (normalize (car t)) (normalize (cdr t))))
    (else t)))

(define (sace-compile blocks)
  (let ((nb (normalize blocks)))
    (string-join (apply append (map compile-block nb)) "\n" (quote infix))))

(define (sace-compile-text text) (sace-compile (sace-parse text)))
