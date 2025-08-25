(define-test-module (test sph sace)
  (import (sph test) (sph sace))

  (define-test (sace-compile inp exp)
    (let (a (sace-compile inp)) (if (string-contains a "\n") (string-split a #\newline) a)))

  (test-execute-procedures-lambda
    (sace-compile
      ((a (is b))) "a is b."
      ((a (is c (from x) (to y)))) "a is c from x to y."
      ((a (is c (from ("any of" (x) (y))) (to z)))) "a is c from x or y to z."
      ((a (is ("all of" (b) (c))))) "a is b and c."
      ((a (is ("any of" (b) (c))))) "a is b or c."
      ((a (is (either (b) (c))))) "a is either b or c."
      ((a (is (not (b) (c))))) "a is not b and c."
      ((a (p) (and (q)))) "a p and q."
      ((a (and (p)))) "a p."
      ((a (o (is p)))) "the o of a is p."
      ((a ("can be" ("any of" (b) (c))))) "a can be b or c."
      ((a (if ("any of" (b) (c)) (then d) (else e)))) "if b or c then a d else a e."
      ((a (if ("all of" (b) (c)) (then (is d)) (else (is e))))) "if b and c then a is d else a is e."
      ((a (o (is p (with q))) (r (is s (by t))) (is u (from v) (to w)))) "the o of a is p with q."
      ((a (o (is p (with q))) (r (is s (by t))) (is u (from v) (to w)))) "the r of a is s by t."
      ((a (o (is p (with q))) (r (is s (by t))) (is u (from v) (to w)))) "a is u from v to w."
      ((a.)) "a.")))
