(define-module (sph sace-ir2) #:export (sace-ir2))
(use-modules (ice-9 match) (srfi srfi-1) (srfi srfi-13) (sph))
(define wh-prefix-list (quote ("which " "who " "what " "where " "when " "why " "how ")))
(define numeric-determiner-list (quote ("exactly" "at least" "at most" "more than" "less than")))
(define nonnumeric-determiner-list (quote ("every" "each" "no")))
(define (starts-with-any? text prefix-list) (any (lambda (p) (string-prefix? p text)) prefix-list))

(define (join-head-tail parts)
  (match parts (() "") ((head . tail) (string-join (cons head tail) " "))))

(define (partition-rels-preds strings)
  (let loop ((xs strings) (rels (quote ())) (preds (quote ())))
    (match xs (() (cons (reverse rels) (reverse preds)))
      ( (x . more)
        (if (string-prefix? "that " x) (loop more (cons x rels) preds)
          (loop more rels (cons x preds)))))))

(define (collect-coordination-items child-nodes)
  (let*
    ( (has-and
        (find (lambda (n) (and (pair? n) (string? (car n)) (string=? (car n) "and"))) child-nodes))
      (has-or
        (find (lambda (n) (and (pair? n) (string? (car n)) (string=? (car n) "or"))) child-nodes))
      (has-either
        (find (lambda (n) (and (pair? n) (string? (car n)) (string=? (car n) "either")))
          child-nodes))
      (conj-symbol
        (cond (has-and (quote and)) (has-or (quote or)) (has-either (quote either)) (else #f))))
    (and conj-symbol
      (let
        ( (pull
            (lambda (n)
              (match n
                ( ( (? string? h) . xs)
                  (if (string=? h (symbol->string conj-symbol)) xs
                    (list (join-head-tail (cons h xs)))))
                ((? string? s) (list s))))))
        (cons conj-symbol (append-map pull child-nodes))))))

(define (compile-item subject-string item)
  (match item
    (("is" ("and" xs ...)) (list (list (quote coord) subject-string "is" (quote and) xs)))
    (("is" ("or" xs ...)) (list (list (quote coord) subject-string "is" (quote or) xs)))
    ( ("is" ("either" a b))
      (list (list (quote coord) subject-string "is" (quote either) (list a b))))
    (("can be" ("and" xs ...)) (list (list (quote coord) subject-string "can be" (quote and) xs)))
    (("can be" ("or" xs ...)) (list (list (quote coord) subject-string "can be" (quote or) xs)))
    ( ("can be" ("either" a b))
      (list (list (quote coord) subject-string "can be" (quote either) (list a b))))
    ;; strings
    ( (? string? text)
      (if (starts-with-any? subject-string wh-prefix-list)
        (list (list (quote question) subject-string text))
        (list (list (quote simple) subject-string text))))
    ( ( (? string? head) . tail)
      (cond
        ( (string=? head "is")
          (let ((coord (collect-coordination-items tail)))
            (if coord
              (let ((conj (car coord)) (items (cdr coord)))
                (list (list (quote coord) subject-string "is" conj items)))
              (list
                (list (quote simple) subject-string
                  (string-join
                    (cons "is" (map (lambda (x) (if (string? x) x (join-head-tail x))) tail)) " "))))))
        ( (string=? head "can be")
          (let ((coord (collect-coordination-items tail)))
            (if coord
              (let ((conj (car coord)) (items (cdr coord)))
                (list (list (quote coord) subject-string "can be" conj items)))
              (list
                (list (quote simple) subject-string
                  (string-join
                    (cons "can be" (map (lambda (x) (if (string? x) x (join-head-tail x))) tail)) " "))))))
        ( (string=? head "iff")
          (match tail ((a b) (list (list (quote iff) subject-string a b)))
            (_ (list (list (quote simple) subject-string (join-head-tail (cons head tail)))))))
        ( (or (member head numeric-determiner-list) (member head nonnumeric-determiner-list))
          (let*
            ( (num+rest (if (member head numeric-determiner-list) tail (cons "" tail)))
              (num (car num+rest)) (rest (cdr num+rest))
              (rels+preds (partition-rels-preds rest)) (rels (car rels+preds))
              (preds (cdr rels+preds)))
            (list (list (quote quant) subject-string head num rels preds))))
        (else (list (list (quote simple) subject-string (join-head-tail (cons head tail)))))))
    (_ (quote ()))))

(define (extract-conditional-items items)
  (let*
    ( (if-node (find (lambda (x) (and (pair? x) (string? (car x)) (string=? (car x) "if"))) items))
      (then-node
        (find (lambda (x) (and (pair? x) (string? (car x)) (string=? (car x) "then"))) items))
      (else-node
        (find (lambda (x) (and (pair? x) (string? (car x)) (string=? (car x) "else"))) items))
      (cond-text (and if-node (cadr if-node))) (then-text (and then-node (cadr then-node)))
      (else-text (and else-node (cadr else-node)))
      (remaining-items
        (filter (lambda (x) (not (or (eq? x if-node) (eq? x then-node) (eq? x else-node)))) items)))
    (list cond-text then-text else-text remaining-items)))

(define (compile-block block)
  (match block
    ( (subject-string . item-list)
      (let*
        ( (bundle (extract-conditional-items item-list)) (cond-text (list-ref bundle 0))
          (then-text (list-ref bundle 1)) (else-text (list-ref bundle 2))
          (remaining-items (list-ref bundle 3))
          (conditional-node
            (if cond-text (list (list (quote if) subject-string cond-text then-text else-text))
              (quote ())))
          (other-nodes (append-map (lambda (it) (compile-item subject-string it)) remaining-items)))
        (append conditional-node other-nodes)))
    (_ (quote ()))))

(define (sace-ir2 forest) (append-map compile-block forest))
