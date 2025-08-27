(define-test-module (test sph sace)
  (import (sph test) (sph sace-ir1) (sph sace-ir2) (sph sace) (sph indent-tree))

  (define-test (sace-ir1 inp exp) (sace-ir1 (list inp)))
  (define-test (sace-ir2 inp exp) (sace-ir2 (list inp)))
  (define-test (sace-compile inp exp) (sace-compile (list inp)))

  (test-execute-procedures-lambda
    (parse-indent-tree "user\n  at least\n    2\n    has s\n    can l"
      (("user" ("at least" "2" "has s" "can l"))))
    (sace-ir1 ("user is" "a" "b") (("user" ("is" "a" "b")))
      ("user" ("is" "a" "b")) (("user" ("is" "a" "b")))
      ("y can be" "json" "xml") (("y" ("can be" "json" "xml")))
      ("resource exactly" "1" "is required") (("resource" ("exactly" "1" "is required")))
      ("user at least" "2" "has sessions" "can log in")
      (("user" ("at least" "2" "has sessions" "can log in")))
      ("x" "if condition" "then occurs" "else is skipped")
      (("x" ("if" "condition") ("then" "occurs") ("else" "is skipped"))) ("x" "if")
      (("x" "if")) ("a is" "and red" "large")
      (("a" ("is" ("and" "red") "large"))) ("a is" ("and red" "light") "large")
      (("a" ("is" ("and" "red" "light") "large"))) ("a is" "all of red" "large")
      (("a" ("is" ("all of" "red") "large"))) ("a is" "any of json" "xml")
      (("a" ("is" ("any of" "json") "xml"))) ("b" "not valid")
      (("b" ("not" "valid"))) ("user has" "role admin")
      (("user" ("has" "role admin"))) ("which user" "has session")
      (("which user" "has session")) ("x" "and") (("x" "and")) ("user" "active") (("user" "active")))
    (sace-ir2 ("user" ("is" ("and" "red" "large" "light")))
      ((coord "user" "is" and ("red" "large" "light"))) ("y" ("can be" ("or" "json" "xml")))
      ((coord "y" "can be" or ("json" "xml"))) ("api" ("can be" ("either" "v1" "v2")))
      ((coord "api" "can be" either ("v1" "v2"))) ("user" ("is" "active"))
      ((simple "user" "is active")) ("x" ("can be" "fast"))
      ((simple "x" "can be fast")) ("user" ("exactly" "1" "that the admin owns" "is required"))
      ((quant "user" "exactly" "1" ("that the admin owns") ("is required")))
      ("resource" ("at least" "2" "have sessions" "can log in"))
      ((quant "resource" "at least" "2" () ("have sessions" "can log in")))
      ("feature" ("iff" "flag is set" "mode is enabled"))
      ((iff "feature" "flag is set" "mode is enabled"))
      ("handler" ("if" "db fails") ("then" "retries") ("else" "aborts"))
      ((if "handler" "db fails" "retries" "aborts"))
      ("worker" ("if" "queue empty") ("then" "sleeps")) ((if "worker" "queue empty" "sleeps" #f))
      ("which user" "has an active session") ((question "which user" "has an active session"))
      ("user" "can log in") ((simple "user" "can log in"))
      ("user" ("has" "role admin") ("is" "active"))
      ((simple "user" "has role admin") (simple "user" "is active"))
      ("a" ("is" ("and" "red") "light" "large")) ((coord "a" "is" and ("red" "light" "large"))))
    (sace-compile ("user" ("is" ("and" "red" "large" "light"))) "user is red and large and light."
      ("y" ("can be" ("or" "json" "xml"))) "y can be json or xml."
      ("api" ("can be" ("either" "v1" "v2"))) "api can be either v1 or v2."
      ("user" ("is" "active")) "user is active."
      ("x" ("can be" "fast")) "x can be fast."
      ("user" ("exactly" "1" "that the admin owns" "is required"))
      "exactly 1 user that the admin owns is required."
      ("resource" ("at least" "2" "have sessions" "can log in"))
      "at least 2 resource have sessions.\nat least 2 resource can log in."
      ("feature" ("iff" "flag is set" "mode is enabled"))
      "feature flag is set if and only if mode is enabled."
      ("handler" ("if" "db fails") ("then" "retries") ("else" "aborts"))
      "if db fails then handler retries else handler aborts."
      ("worker" ("if" "queue empty") ("then" "sleeps")) "if queue empty then worker sleeps."
      ("which user" "has an active session") "which user has an active session?"
      ("user" ("has" "role admin") ("is" "active")) "user has role admin.\nuser is active."
      ("a" ("is" ("and" "red") "light" "large")) "a is red and light and large.")))
