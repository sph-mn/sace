# sace
an indented plaintext syntax that compiles to [attempto controlled english](https://en.wikipedia.org/wiki/Attempto_Controlled_English).
it reduces repetition via subject scoping, uses keywords for logical forms, and preserves aces determinism.
the syntax is a tree: indentation encodes scope, paths through the tree generate sentences.

## example
input:
```
users
  at least
    2
    have sessions
    can log in
user
  exactly
    1
    can write
resource
  exactly
    1
    that the admin owns
    is required
feature
  iff
    flag is set
    mode is enabled
the system
  is all of
    scalable
    reliable
  output is any of
    json
    xml
the error handling
  if
    any of
      the request is invalid
      the database connection fails
  then occurs
  else is skipped
which user
  has an active session
the api
  can be either
    v1
```

output:
```
at least 2 users have sessions.
at least 2 users can log in.
exactly 1 user can write.
exactly 1 resource that the admin owns is required.
feature flag is set if and only if mode is enabled.
the system is scalable and reliable.
the system output is json or xml.
if any of the request is invalid or the database connection fails then the error handling occurs else the error handling is skipped.
which user has an active session?
the api can be either v1 or v2.
```

## syntax
```
program := block+
block := subject line item*
subject := line not starting with indent or preposition
item := indent chain
chain := head (head ...)* tail
tail := "." | item* | list
list := operator item+
operator := and | or | either | all of | any of | not | if | then | else | iff
head := verb-phrase | preposition | "can be"
preposition := from | to | with | by | for | within | of | at | before | after
leaf := noun-phrase | adjective | code-token | number
```

### semantics
* a path is a sequence of heads ending in a leaf.
* indentation defines scope continuation.
* siblings under a head -> multiple sentences (multiplication).
* keywords alter path processing:
  * `and` / `or` / `either`: coordination inside one sentence
  * `all of`: universal quantification, expands across items
  * `any of`: existential quantification
  * `not`: negation
  * `if` / `then` / `else`: conditional sentence
  * `iff`: biconditional, "if and only if"
* determiners (`exactly`, `at least`, `at most`, `more than`, `less than`, `every`, `each`, `no`) scope quantified noun phrases.

### structural examples

```
x is and
  red
  large
```

-> "x is red and large."

```
y can be any of
  json
  xml
```

-> "y can be json or xml."

```
user at least
  2
  has sessions
  can log in
```

-> "at least 2 users have sessions."
-> "at least 2 users can log in."

```
resource exactly
  1
  that the admin
  owns
```

-> "exactly 1 resource that the admin owns."

```
feature iff
  flag is set
  mode is enabled
```

-> "feature flag is set if and only if mode is enabled."

```
error handling if
  request is invalid
then occurs
else is skipped
```

-> "if request is invalid then error handling occurs else error handling is skipped."

```
which user
  has active session
```

-> "which user has active session?"

```

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

# possible enhancements
* output only valid ace
* parse ace