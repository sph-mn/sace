# sace
an indented plaintext syntax that compiles to [attempto controlled english](https://en.wikipedia.org/wiki/Attempto_Controlled_English).
it reduces repetition via subject scoping, uses keywords for logical forms, and preserves aces determinism.
the syntax is a tree: indentation encodes scope, paths through the tree generate sentences.

## example
input:
```
the web service api
  is a client-server interface.
  is a hierarchy of endpoints
    from http requests
    to json responses.
  is and
    stateless
    cacheable.
  adheres to a restful design style
    and is consistent across resources.
  output is idempotent.
  evaluation is event-driven.
request handler
  prepares a response object
    for the client.
error handling
  occurs if or
    the request is invalid
    the database connection fails.
  can be or
    400 bad request
    500 internal server error.
each endpoint of the web service api
  is unique.
```
output:
```
the web service api is a client-server interface.
the web service api is a hierarchy of endpoints from http requests to json responses.
the web service api is stateless and cacheable.
the web service api adheres to a restful design style and is consistent across resources.
the output of the web service api is idempotent.
the evaluation of the web service api is event-driven.
the request handler prepares a response object for the client.
the error handling occurs if the request is invalid or the database connection fails.
the error handling can be 400 bad request or 500 internal server error.
each endpoint of the web service api is unique.
```

## syntax
```
program := block+
block := subject-line item+
subject-line := line not starting with indent or preposition
item := indent chain
chain := head (head ...)* tail
tail := "." | item+ | list
list := operator item+
operator := and | or | either | all | some | not | if | then | else
head := verb-phrase | preposition | "can be"
preposition := from | to | with | by | for | within | of | at | before | after
leaf := noun-phrase | adjective | code-token
```

### semantics
* a path is a sequence of heads ending in a leaf.
* indentation defines scope continuation.
* siblings under a head -> multiple sentences (multiplication).
* keywords alter path processing:
  * `and` / `or` / `either`: coordination inside one sentence
  * `all`: universal quantification, expands across items
  * `some`: existential quantification
  * `not`: negation
  * `if` / `then` / `else`: conditional sentence

### structural examples
```
x is and
  red
  large.
```
-> "x is red and large."
```
y can be or
  case a
  case b.
```
-> "y can be case a or case b."
```
z occurs if
  cond.
```
-> "z occurs if cond."
```
z occurs if or
  cond1
  cond2
then action
else alternative.
```
-> "if cond1 or cond2 then z action else z alternative."

## benefits
* abstract syntax tree: clean separation between authoring and ace output.
* multiple backends possible: ace, fol, sql-like constraints.
* round-tripping: ace ↔ sace with normalization.
* machine processing: tree is trivial to traverse, transform, normalize.
* composability: trees can be generated programmatically.
* determinism: logical forms are explicit; no ambiguity.

## implementation
dependencies: guile 3+

### from shell
~~~
cat example.sace | ./exe/sace
~~~

### from scheme
```scheme
(add-to-load-path (string-append (dirname (current-filename)) "/modules"))
(use-modules (sph sace))

(define example
"the test subject
  is simple.
")

(for-each (lambda (s) (display s) (newline)) (sace-compile-text example))
```

run:
```
guile example.scm
```