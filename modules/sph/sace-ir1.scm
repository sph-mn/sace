(define-module (sph sace-ir1) #:export (sace-ir1 sace-ir1-text))
(use-modules (ice-9 match) (srfi srfi-1) (srfi srfi-13) (sph indent-tree))

(define leading-keyword-list
  (quote ("if" "then" "else" "iff" "and" "or" "either" "all of" "any of" "not")))

(define trailing-keyword-list
  (quote ("can be" "at least" "at most" "more than" "less than" "exactly" "is" "has")))

(define (split-leading-keyword full-string keyword-list)
  (let loop ((remaining-keywords keyword-list))
    (if (null? remaining-keywords) #f
      (let*
        ( (candidate-keyword (car remaining-keywords))
          (pattern (string-append candidate-keyword " ")))
        (if (string-prefix? pattern full-string)
          (let*
            ( (rest-start (string-length pattern))
              (rest-string
                (string-trim-both (substring full-string rest-start (string-length full-string)))))
            (if (string-null? rest-string) #f (cons candidate-keyword rest-string)))
          (loop (cdr remaining-keywords)))))))

(define (split-trailing-keyword full-string keyword-list)
  (let loop ((remaining-keywords keyword-list))
    (if (null? remaining-keywords) #f
      (let*
        ( (candidate-keyword (car remaining-keywords))
          (pattern (string-append " " candidate-keyword)))
        (if (string-suffix? pattern full-string)
          (let*
            ( (prefix-end (- (string-length full-string) (string-length pattern)))
              (prefix-string (string-take full-string prefix-end))
              (prefix-trimmed (string-trim-right prefix-string)))
            (if (string-null? prefix-trimmed) #f (cons prefix-trimmed candidate-keyword)))
          (loop (cdr remaining-keywords)))))))

(define (split-keywords-in-item item)
  (match item
    ( (? string? leaf-string)
      (let ((match-result (split-leading-keyword leaf-string leading-keyword-list)))
        (if match-result
          (let ((keyword (car match-result)) (rest (cdr match-result))) (list keyword rest))
          leaf-string)))
    ((? pair? nested-block) (split-keywords-in-block nested-block)) (else item)))

(define (split-keywords-in-block block)
  (match block
    ( (subject-string . item-list)
      (let*
        ( (normalized-items (map split-keywords-in-item item-list))
          (subject-trailing
            (and (string? subject-string)
              (split-trailing-keyword subject-string trailing-keyword-list))))
        (if subject-trailing
          (let ((subject-prefix (car subject-trailing)) (keyword (cdr subject-trailing)))
            (list subject-prefix (cons keyword normalized-items)))
          (let
            ( (subject-leading
                (and (string? subject-string)
                  (split-leading-keyword subject-string leading-keyword-list))))
            (if subject-leading
              (let ((keyword (car subject-leading)) (rest (cdr subject-leading)))
                (cons keyword (cons rest normalized-items)))
              (cons subject-string normalized-items))))))
    (else block)))

(define (sace-ir1 forest) (map split-keywords-in-block forest))
(define (sace-ir1-text text) (sace-ir1 (parse-indent-tree text)))
