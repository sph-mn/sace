(define-module (sph sace-ir2) #:export (sace-ir2))
(use-modules (ice-9 match) (srfi srfi-1) (srfi srfi-13))

(define sph-sace-ir2-description
  "SACE-IR2 rewrites keyword-marked trees into normalized IR nodes.
   (\"user\" (\"is\" (\"and\" \"red\" \"large\"))) -> ((coord \"user\" \"is\" and (\"red\" \"large\")))
   (\"user\" (\"exactly\" \"1\" \"that the admin owns\" \"is required\")) -> ((quant \"user\" \"exactly\" \"1\" (\"that the admin owns\") (\"is required\")))
   (\"feature\" (\"iff\" \"flag is set\" \"mode is enabled\")) -> -> ((iff \"feature\" \"flag is set\" \"mode is enabled\"))")

(define wh-prefix-list (quote ("which " "who " "what " "where " "when " "why " "how ")))
(define numeric-determiner-list (quote ("exactly" "at least" "at most" "more than" "less than")))
(define nonnumeric-determiner-list (quote ("every" "each" "no")))
(define (starts-with-any? s ps) (any (lambda (p) (string-prefix? p s)) ps))

(define (flatten-strings xs)
  (append-map (lambda (x) (if (string? x) (list x) (flatten-strings x))) xs))

(define (join-head-tail parts) (string-join (flatten-strings parts) " "))
(define (item->text x) (if (string? x) x (join-head-tail x)))

(define (partition-rels-preds strs)
  (let loop ((xs strs) (rels (quote ())) (preds (quote ())))
    (match xs (() (cons (reverse rels) (reverse preds)))
      ( (x . more)
        (if (string-prefix? "that " x) (loop more (cons x rels) preds)
          (loop more rels (cons x preds)))))))

(define (compile-item subject item)
  (match item (("is" ("and" xs ...)) (list (list (quote coord) subject "is" (quote and) xs)))
    (("is" ("or" xs ...)) (list (list (quote coord) subject "is" (quote or) xs)))
    (("is" ("either" a b)) (list (list (quote coord) subject "is" (quote either) (list a b))))
    (("is" ("any of" xs ...)) (list (list (quote coord) subject "is" (quote or) xs)))
    (("is" ("all of" xs ...)) (list (list (quote coord) subject "is" (quote and) xs)))
    (("can be" ("and" xs ...)) (list (list (quote coord) subject "can be" (quote and) xs)))
    (("can be" ("or" xs ...)) (list (list (quote coord) subject "can be" (quote or) xs)))
    ( ("can be" ("either" a b))
      (list (list (quote coord) subject "can be" (quote either) (list a b))))
    (("can be" ("any of" xs ...)) (list (list (quote coord) subject "can be" (quote or) xs)))
    (("can be" ("all of" xs ...)) (list (list (quote coord) subject "can be" (quote and) xs)))
    ( (? string? text)
      (if (starts-with-any? subject wh-prefix-list) (list (list (quote question) subject text))
        (list (list (quote simple) subject text))))
    ( ( (? string? head) . tail)
      (cond
        ( (string=? head "is")
          (let ((all-strings? (every string? tail)))
            (if (and all-strings? (> (length tail) 1))
              (map (lambda (t) (list (quote simple) subject (string-append "is " t))) tail)
              (list
                (list (quote simple) subject
                  (string-join
                    (cons "is" (map (lambda (x) (if (string? x) x (join-head-tail x))) tail)) " "))))))
        ( (string=? head "can be")
          (let ((all-strings? (every string? tail)))
            (if (and all-strings? (> (length tail) 1))
              (map (lambda (t) (list (quote simple) subject (string-append "can be " t))) tail)
              (list
                (list (quote simple) subject
                  (string-join
                    (cons "can be" (map (lambda (x) (if (string? x) x (join-head-tail x))) tail)) " "))))))
        ( (string=? head "iff")
          (match tail ((a b) (list (list (quote iff) subject a b)))
            (_ (list (list (quote simple) subject (join-head-tail (cons head tail)))))))
        ( (or (member head numeric-determiner-list) (member head nonnumeric-determiner-list))
          (let*
            ( (num+rest (if (member head numeric-determiner-list) tail (cons "" tail)))
              (num (car num+rest)) (rest (cdr num+rest))
              (rp (partition-rels-preds rest)) (rels (car rp)) (preds (cdr rp)))
            (list (list (quote quant) subject head num rels preds))))
        (else
          (let ((tail-all-strings? (every string? tail)))
            (if tail-all-strings?
              (list (list (quote simple) subject (string-join (cons head tail) " ")))
              (append-map (lambda (it) (compile-item (string-append subject " " head) it)) tail))))))
    (_ (quote ()))))

(define (extract-conditional-items items)
  (let*
    ( (if-nodes
        (filter (lambda (x) (and (pair? x) (string? (car x)) (string=? (car x) "if"))) items))
      (then-nodes
        (filter (lambda (x) (and (pair? x) (string? (car x)) (string=? (car x) "then"))) items))
      (else-nodes
        (filter (lambda (x) (and (pair? x) (string? (car x)) (string=? (car x) "else"))) items))
      (cond-text (and (pair? if-nodes) (cadr (car if-nodes)))) (then-payloads (map cdr then-nodes))
      (else-payloads (map cdr else-nodes))
      (remaining (lset-difference eq? items (append if-nodes then-nodes else-nodes))))
    (values cond-text then-payloads else-payloads remaining)))

(define (insert-after s i add)
  (string-append (substring s 0 i) add (substring s i (string-length s))))

(define (negate-cond c)
  (cond
    ((string-contains c " is not ") (string-replace-substring c " is not " " is "))
    ((string-contains c " can not be ") (string-replace-substring c " can not be " " can be "))
    ((string-contains c " does not have ") (string-replace-substring c " does not have " " has "))
    ( (let ((i (string-contains c " is ")))
        (and i (string-append (substring c 0 (+ i 4)) "not " (substring c (+ i 4))))))
    ( (let ((i (string-contains c " can be ")))
        (and i (string-append (substring c 0 (+ i 5)) " not" (substring c (+ i 5))))))
    ( (let ((i (string-contains c " has ")))
        (and i (string-append (substring c 0 i) " does not have " (substring c (+ i 5))))))
    (else #f)))

(define (ir->predicate subject-string ir-node)
  (match ir-node (((quote simple) s t) (if (string=? s subject-string) (list t) (quote ())))
    ( ( (quote coord) s verb conj items)
      (if (string=? s subject-string)
        (let*
          ( (xs (map string-trim-both items))
            (body
              (case conj
                ((and) (string-join xs " and "))
                ((or) (string-join xs " or "))
                ( (either)
                  (if (= (length xs) 2) (string-append "either " (car xs) " or " (cadr xs))
                    (string-join xs " or ")))
                (else (string-join xs " ")))))
          (list (string-append verb " " body)))
        (quote ())))
    (_ (quote ()))))

(define (items-subtrees->pred-strings subject-string payloads)
  (append-map
    (lambda (payload)
      (append-map (lambda (node) (ir->predicate subject-string node))
        (append-map (lambda (it) (compile-item subject-string it)) payload)))
    payloads))

(define (payloads->explicit-else-ifs subject-string payloads)
  (append-map
    (lambda (payload)
      (filter (lambda (n) (and (pair? n) (eq? (car n) (quote if))))
        (compile-block (cons subject-string payload))))
    payloads))

(define (compile-block block)
  (match block (("iff" a b) (list (list (quote iff) "" (item->text a) (item->text b))))
    (("iff" a b . _) (list (list (quote iff) "" (item->text a) (item->text b))))
    ( (subject-string . item-list)
      (call-with-values (lambda () (extract-conditional-items item-list))
        (lambda (cond-text then-payloads else-payloads remaining-items)
          (let*
            ( (then-strings (items-subtrees->pred-strings subject-string then-payloads))
              (bare-else-strings (items-subtrees->pred-strings subject-string else-payloads))
              (explicit-else-if-nodes (payloads->explicit-else-ifs subject-string else-payloads))
              (negated (and cond-text (negate-cond cond-text)))
              (if-then-nodes
                (if cond-text
                  (map (lambda (t) (list (quote if) subject-string cond-text t #f)) then-strings)
                  (quote ())))
              (if-else-nodes
                (cond
                  ( (and cond-text negated (pair? bare-else-strings))
                    (map (lambda (e) (list (quote if) subject-string negated e #f))
                      bare-else-strings))
                  (else (quote ()))))
              (other-nodes
                (append-map (lambda (it) (compile-item subject-string it)) remaining-items)))
            (append if-then-nodes explicit-else-if-nodes if-else-nodes other-nodes)))))
    (_ (quote ()))))

(define (sace-ir2 forest) (append-map compile-block forest))
