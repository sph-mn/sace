(add-to-load-path (string-append (dirname (current-filename)) "/modules"))
(use-modules (sph sace))

(define example-text  "the web service api
  is a hierarchy of endpoints
    from http requests
    to json responses.
the web service api
  is and
    stateless
    cacheable.
the error handling
  occurs if or
    the request is invalid
    the database connection fails.
the error handling
  can be or
    400 bad request
    500 internal server error.)

(for-each (lambda (s) (display s) (newline)) (sace-compile example-text))
