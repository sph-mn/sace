(define-module (sph sace) #:export (sace-compile-text sace-compile sace-lin sace-lin-join))

(use-modules (ice-9 match) (srfi srfi-1)
  (srfi srfi-13) (sph indent-tree) (sph sace-ir1) (sph sace-ir2))

(define (emit-sentence text end-char) (string-append text end-char))

(define (emit-space-joined a b)
  (if (or (string-null? a) (string-null? b)) (string-append a b) (string-append a " " b)))

(define (lin-simple subject-string text)
  (emit-sentence (emit-space-joined subject-string (string-trim-both text)) "."))

(define (lin-coord subject-string verb-string conj-symbol item-list)
  (let*
    ( (items (map string-trim-both item-list))
      (body
        (case conj-symbol
          ((and) (string-join items " and "))
          ((or) (string-join items " or "))
          ( (either)
            (if (= (length items) 2) (string-append "either " (car items) " or " (cadr items))
              (string-join items " or ")))
          (else (string-join items " "))))
      (text (string-append subject-string " " verb-string " " body)))
    (emit-sentence text ".")))

(define (lin-quant subject-string determiner num-string rel-list pred-list)
  (let*
    ( (rels
        (if (null? rel-list) ""
          (string-append " " (string-join (map string-trim-both rel-list) " "))))
      (np (string-append determiner " " (string-trim-both num-string) " " subject-string rels)))
    (if (null? pred-list) (list (emit-sentence np "."))
      (map (lambda (p) (emit-sentence (string-append np " " (string-trim-both p)) ".")) pred-list))))

(define (lin-iff subject-string a b)
  (emit-sentence
    (string-append (emit-space-joined (string-trim-both subject-string) (string-trim-both a))
      " if and only if " (string-trim-both b))
    "."))

(define (lin-if subject-string cond-text then-text else-text)
  (let
    ( (then-clause
        (and then-text (string-append " then " subject-string " " (string-trim-both then-text))))
      (else-clause
        (and else-text (string-append " else " subject-string " " (string-trim-both else-text)))))
    (emit-sentence
      (string-append "if " (string-trim-both cond-text) (or then-clause "") (or else-clause "")) ".")))

(define (lin-question subject-string text)
  (emit-sentence (emit-space-joined subject-string (string-trim-both text)) "?"))

(define (sace-lin ir-nodes)
  (append-map
    (lambda (node)
      (match node (((quote simple) subject-string text) (list (lin-simple subject-string text)))
        ( ( (quote coord) subject-string verb-string conj-symbol item-list)
          (list (lin-coord subject-string verb-string conj-symbol item-list)))
        ( ( (quote quant) subject-string determiner num-string rel-list pred-list)
          (lin-quant subject-string determiner num-string rel-list pred-list))
        (((quote iff) subject-string a b) (list (lin-iff subject-string a b)))
        ( ( (quote if) subject-string cond-text then-text else-text)
          (list (lin-if subject-string cond-text then-text else-text)))
        (((quote question) subject-string text) (list (lin-question subject-string text)))
        (else (quote ()))))
    ir-nodes))

(define (sace-lin-join ir-nodes) (string-join (sace-lin ir-nodes) "\n"))
(define (sace-compile blocks) (sace-lin-join (sace-ir2 (sace-ir1 blocks))))
(define (sace-compile-text text) (sace-compile (parse-indent-tree text)))
