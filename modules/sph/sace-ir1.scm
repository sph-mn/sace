(define-module (sph sace-ir1) #:export (sace-ir1 sace-ir1-text))
(use-modules (ice-9 match) (srfi srfi-1) (srfi srfi-13) (sph indent-tree))

(define sph-sace-ir1-description
  "sace-ir1 splits off keywords from subject or item strings.
   (\"user is\" \"active\") -> ((\"user\" (\"is\" \"active\")))
   (\"a\" (\"is and\" \"red\" \"large\")) -> ((\"a\" (\"is\" (\"and\" \"red\" \"large\"))))")

(define leading-keyword-list
  (quote ("if" "then" "else" "iff" "and" "or" "either" "any of" "all of" "not")))

(define trailing-keyword-list
  (quote ("can be" "at least" "at most" "more than" "less than" "exactly" "is" "has")))

(define composite-head-map
  (quote
    ( ("is and" "is" . "and") ("is or" "is" . "or") ("is any of" "is" . "any of")
      ("is all of" "is" . "all of") ("can be and" "can be" . "and")
      ("can be or" "can be" . "or") ("can be either" "can be" . "either")
      ("can be any of" "can be" . "any of") ("can be all of" "can be" . "all of"))))

(define composite-keys (map car composite-head-map))

(define (split-suffix-composite s)
  (let loop ((ks composite-keys))
    (if (null? ks) #f
      (let* ((key (car ks)) (pat (string-append " " key)))
        (if (string-suffix? pat s)
          (let*
            ( (pre-end (- (string-length s) (string-length pat)))
              (pre (string-trim-right (string-take s pre-end))))
            (if (string-null? pre) #f (cons pre key)))
          (loop (cdr ks)))))))

(define (split-leading-keyword full-string keyword-list)
  (let loop ((rest-list keyword-list))
    (if (null? rest-list) #f
      (let* ((kw (car rest-list)) (pat (string-append kw " ")))
        (if (string-prefix? pat full-string)
          (let*
            ( (i (string-length pat))
              (tail (string-trim-both (substring full-string i (string-length full-string)))))
            (if (string-null? tail) #f (cons kw tail)))
          (loop (cdr rest-list)))))))

(define (split-trailing-keyword full-string keyword-list)
  (let loop ((rest-list keyword-list))
    (if (null? rest-list) #f
      (let* ((kw (car rest-list)) (pat (string-append " " kw)))
        (if (string-suffix? pat full-string)
          (let*
            ( (pre-end (- (string-length full-string) (string-length pat)))
              (pre (string-trim-right (string-take full-string pre-end))))
            (if (string-null? pre) #f (cons pre kw)))
          (loop (cdr rest-list)))))))

(define (split-keywords-in-item item)
  (match item
    ( (? string? s)
      (let ((m (split-leading-keyword s leading-keyword-list))) (if m (list (car m) (cdr m)) s)))
    ((? pair? blk) (split-keywords-in-block blk)) (else item)))

(define (pack-composite-heads items)
  (let loop ((xs items) (acc (quote ())))
    (match xs (() (reverse acc))
      ( (first . rest)
        (cond
          ( (string? first)
            (let ((exact (assoc first composite-head-map)) (suf (split-suffix-composite first)))
              (cond
                (exact
                  (let*
                    ( (verb (car (cdr exact))) (conj (cdr (cdr exact)))
                      (children (take-while string? rest)) (remain (drop-while string? rest)))
                    (if (null? children) (loop rest (cons first acc))
                      (loop remain (cons (list verb (cons conj children)) acc)))))
                (suf
                  (let*
                    ( (prefix (car suf)) (ckey (cdr suf)) (entry (assoc ckey composite-head-map))
                      (verb (car (cdr entry))) (conj (cdr (cdr entry)))
                      (children (take-while string? rest)) (remain (drop-while string? rest)))
                    (if (null? children) (loop rest (cons first acc))
                      (loop remain (cons (list prefix (list verb (cons conj children))) acc)))))
                (else (loop rest (cons first acc))))))
          ( (and (pair? first) (string? (car first)))
            (let*
              ( (head (car first)) (entry (assoc head composite-head-map))
                (suf (and (not entry) (split-suffix-composite head))))
              (cond
                (entry
                  (let* ((verb (car (cdr entry))) (conj (cdr (cdr entry))) (children (cdr first)))
                    (loop rest (cons (list verb (cons conj children)) acc))))
                (suf
                  (let*
                    ( (prefix (car suf)) (ckey (cdr suf)) (e2 (assoc ckey composite-head-map))
                      (verb (car (cdr e2))) (conj (cdr (cdr e2))) (children (cdr first)))
                    (loop rest (cons (list prefix (list verb (cons conj children))) acc))))
                (else (loop rest (cons first acc))))))
          (else (loop rest (cons first acc))))))))

(define (split-keywords-in-block block)
  (match block
    ( (subject . items)
      (let*
        ( (items* (map split-keywords-in-item items)) (items** (pack-composite-heads items*))
          (subject-trail
            (and (string? subject) (split-trailing-keyword subject trailing-keyword-list))))
        (if subject-trail
          (let ((pre (car subject-trail)) (kw (cdr subject-trail))) (list pre (cons kw items**)))
          (let
            ( (subject-lead
                (and (string? subject) (split-leading-keyword subject leading-keyword-list))))
            (if subject-lead
              (let ((kw (car subject-lead)) (tail (cdr subject-lead)))
                (cons kw (cons tail items**)))
              (cons subject items**))))))
    (else block)))

(define (sace-ir1 forest) (map split-keywords-in-block forest))
(define (sace-ir1-text text) (sace-ir1 (parse-indent-tree text)))
