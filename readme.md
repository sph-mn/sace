# sace
an indented plaintext syntax that can compile to [attempto controlled english](https://en.wikipedia.org/wiki/Attempto_Controlled_English).
it reduces repetition via subject scoping and uses keywords for logical forms.
the syntax is a tree: indentation encodes scope, paths through the tree generate sentences.

the following examples use a form inspired by standard ace; they ignore lexicon and casing requirements.

## example
input:
~~~
the system
  is all of
    scalable
    reliable
  output can be any of
    json
    xml

api
  can be either
    v1
    v2

resource exactly
  1
  that the admin owns
  exists

feature
  iff
    flag is set
    mode is enabled

handler
  if
    database connection is down
  then
    retries
    logs
  else aborts

which user
  has an active session
  is an inactive user
~~~

output:
~~~
the system is scalable and reliable.
the system output can be json or xml.
api can be either v1 or v2.
exactly 1 resource that the admin owns exists.
feature flag is set if and only if mode is enabled.
if database connection is down then handler retries.
if database connection is down then handler logs.
if database connection is not down then handler aborts.
which user has an active session?
which user is an inactive user?
~~~

## syntax

```
program := block+
block := subject item*
subject := line not starting with indent or preposition
item := indent chain
chain := head tail
tail := item* | list
list := operator item+
operator := and | or | either | any of | all of | not | if | then | else | iff
head := verb-phrase | preposition | "can be" | determiner
preposition := from | to | with | by | for | within | of | at | before | after
determiner := exactly | at least | at most | more than | less than | every | each | no
leaf := noun-phrase | adjective | code-token | number
```

## semantics
* a path is a sequence of heads ending in a leaf.
* indentation defines scope continuation.
* siblings under the same head multiply into separate sentences.
* coordinators:
  * `is all of x y …` -> `is x and y …`
  * `can be any of x y …` -> `can be x or y …`
  * `either x y` remains exclusive disjunction.
* conditionals:
  * `if` with optional `then` and `else`. multiple `then` items yield multiple sentences; one structured `then` with `and`/`or` yields one coordinated sentence.
* biconditional:
  * `iff` requires exactly two clauses: `a` and `b` -> “a if and only if b.”
  * allowed either as `subject -> iff -> a b` or top-level `iff -> a b`.
* determiners scope quantified noun phrases; no morphology is inferred.

## structural examples

```
x is and
  red
  large
-> "x is red and large."

y can be any of
  json
  xml
-> "y can be json or xml."

every user
  has active session
-> "every user has active session."

user at least
  2
  have sessions
-> "at least 2 users have sessions."

resource exactly
  1
  that the admin owns
  exists
-> "exactly 1 resource that the admin owns exists."

handler
  if
    the queue is empty
  then sleeps
-> "if the queue is empty then handler sleeps."

iff
  feature flag is set
  mode is enabled
-> "feature flag is set if and only if mode is enabled."
```

## benefits
* abstract syntax tree: clean separation between authoring and ace output.
* multiple backends possible: ace, fol, sql-like constraints.
* round-tripping: ace ↔ sace with normalization.
* machine processing: tree is trivial to traverse, transform, normalize.
* composability: trees can be generated programmatically.

## implementation
dependencies: [guile](https://www.gnu.org/software/guile/) 3+

### from shell
~~~
cat other/example.sace | ./exe/sace
~~~

### from scheme
indented plaintext strings and s-expressions can be processed.
the former with `sace-compile-text` and the latter with `sace-compile`.

```scheme
(use-modules (sph sace))

(define example (quote (("the test subject" "is simple" "is short"))))

(display (sace-compile example))
(newline)
```

run:
```
guile other/example.scm
```

# possible enhancements
* transpile ace to sace
* reject invalid ace output

# links
* [Attempto repositories](https://github.com/Attempto)