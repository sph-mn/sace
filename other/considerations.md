# considerations
~~~
does indentation depth beyond one level carry semantics or only grouping?
  indentation defines continuation of a path from subject head to leaf. deeper levels extend the same path; siblings at the same level create additional paths.
  semantic path model - selected
    treat each root-to-leaf as one sentence; indentation appends heads; siblings multiply sentences. simple, composable, deterministic.
  group-only indentation
    would require explicit path operators elsewhere; loses the concise "path" framing.
how are leaves interpreted in the path model?
  leaves terminate paths. each root-to-leaf emits one ace sentence unless an operator modifies joining or scope.
  leaf-as-tail - selected
    leaves are the tails; punctuation belongs at the leaf. clear termination, easy joining rules.
  leaf-as-node with implicit punctuation
    increases ambiguity on where sentences end; complicates rendering.
should we allow multiword operators like "all of" and "any of"?
  bare "and"/"or" sometimes read ungrammatically when the head is verbal; block lists need stable determiners.
  multiword operator tokens - selected
    recognize "all of" and "any of" as atomic operators; parse-time tokenization; render as ordinary "and"/"or". improves grammar without changing semantics.
  only "and"/"or"
    forces awkward constructions or extra rewriting; fragile with verbal heads.
how to render "all of" and "any of" in ace?
  these are structural cues for block coordination, not target surface strings.
  desugar to "and"/"or" - selected
    "all of" → coordinated "and"; "any of" → coordinated "or". natural ace output.
  preserve "all of"/"any of" lexically
    reads stilted in ace; redundant with coordination.
what is the behavior of single-child coordination nodes?
  trees occasionally contain a degenerate list due to formatting or refactors.
  collapse single-child - selected
    `and/or/all of/any of` with one child reduces to that child. avoids ungrammatical "and x".
  forbid single-child
    cleaner spec but harms ergonomics; requires rewrites during authoring.
how are inline vs block coordinations expressed?
  inline joins are concise; block lists are clearer for multi-item expansions.
  dual form - selected
    inline: "a and b". block: "all of" or "any of" with ≥2 children. uniform semantics, grammar-safe block form.
  single unified form
    either too verbose (always block) or fragile (always inline).
what is the minimal operator set?
  keep syntax small yet expressive and deterministic.
  {and, or, not, all of, any of, if, then, else, either} - selected
    covers coordination, quantification sugar, negation, conditionals, and exclusive choice. deterministic scope.
  larger set with modality/quantifiers
    adds complexity without direct generative benefit at this level.
how are conditionals encoded?
  ace requires explicit "if ... then ..." and optional "else".
  indented if/then/else under the same node - selected
    `if` node with children: conditions, `then` branch, optional `else` branch. subject context is propagated to both branches. single sentence emitted.
  flat "if" plus separate sentences
    risks emitting multiple sentences or losing subject context; violates ace form.
how do prepositions integrate with lists and heads?
  paths often include pp chains; lists appear within or across pps.
  prepositions as normal heads - selected
    `from/to/with/...` are heads that append to the path; lists under them are compiled with the same operator rules. orthogonal, composable.
  special pp-only syntax
    increases grammar surface and parser complexity without added power.
how to express genitive labels like "the x of s is ..."?
  many facts concern a part or attribute of the subject.
  label-as-head with genitive rewrite - selected
    if a non-operator, non-preposition head has a child whose head is "is"/"can be", rewrite context to "the <label> of <subject> ...". compact and readable.
  dedicated "of" blocks
    verbose and redundant; harder to factor.
should bare subject lines with trailing period be allowed?
  authors sometimes want to emit a single sentence line without a block.
  allow subject-only sentence - selected
    a top-level line ending in "." with no items emits as-is. useful for one-offs and complements block style.
  disallow
    forces awkward one-item blocks for trivial sentences.
what is the tokenization rule for multiword heads?
  "can be", "all of", "any of" must not split accidentally.
  greedy multiword matching at parse time - selected
    collapse known multiword operators into single heads before building chains. prevents misparse and keeps compilation simple.
  post-parse normalization
    harder to repair once tree shape is fixed; fragile.
are subjects limited to strings or may they be symbols too?
  tooling often uses symbolic identifiers.
  accept strings or symbols - selected
    coerce symbols to strings at use-sites. zero-cost ergonomics, no extra pass required.
  strings only
    unnecessarily restrictive for generators and tests.
should indentation width be configurable?
  current usage standardizes on two spaces.
  fixed width: 2 spaces - selected
    simpler parser and stable formatting across tools.
  configurable width
    adds parameters and failure modes without clear benefit.
how are paths expanded into sentences by default?
  need a single, predictable rule.
  path-wise expansion - selected
    each root-to-leaf path yields a sentence unless modified by an operator. siblings multiply sentences; operators join or scope within a sentence.
  node-type driven expansion
    more ad hoc rules; weak composability.
keyword inclusion choices
  read from head to leaf, it should as much as possible read like a correct ace sentence.
  sace does no pluralization, no article insertion, no guessing missing verbs.
  alternatives to "and" and "or" ("all of" and "any of") were added to make a very common case more readable ("and"/"or" are infix, they dont compose well as prefixes and there was no alternative)
~~~
