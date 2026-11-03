# `delimiter_doctor` repairs balance without checking nesting

**Type:** bug report
**Component:** `delimiter_doctor`, the delimiter doctor for Racket source files
**Severity:** a repair plan can pass its own simulation and produce code that does not mean
what the author intended

## Summary

The repair plan is validated by delimiter counts alone: an insertion is accepted when the file
then lexes `BALANCED`. Two placements of the same closer can both balance while nesting
differently, and the plan picks by "after the last code token of the previous line", which is
the wrong column whenever that token is an argument of an outer form rather than the last
element of the form being closed.

## Evidence

Editing a test file so that four closers were missing, the doctor proposed:

```text
E1 [medium] insert `)` at 340:27 -- line 341 starts at col 8, at/left of 1 open form(s)
    -                        #f)
    +                        #f))
E2 [medium] insert `)` at 343:27
E3 [medium] insert `)` at 346:27
E4 [medium] insert `)` at 349:27
SIMULATION: after E1..E4 the file lexes BALANCED (weakest edit: medium)
```

Applying all four produced a file the doctor called `BALANCED` and `raco make` accepted, and the
suite then failed at runtime. The location line names the test file being repaired, so it is
elided here -- the reproduction below needs no access to that tree:

```text
type > what a declaration may say > append accepts an identifier or a function literal of any arity
ERROR  name: check-equal?  location: <the test file under repair>:338:8
compile-message: arity mismatch; the expected number of arguments does not match the given number
  expected: 1
  given: 2
```

Each inserted closer belongs at the end of the `declaring` line (339, 342, 345, 348), where it
closes `compile-message` ahead of the `#f` that follows. Placed after `#f`, the closer closes
`compile-message` too late and `#f` becomes its second argument. Both placements balance; only
one parses into the intended tree.

The same skew appears in a reproduction that needs nothing but Racket. One closer is missing --
the one that should close `compile-message` at the end of the `declaring` line:

```racket
#lang racket/base

(define (compile-message s) (list s))
(define (check-equal? a b) (cons a b))
(define (declaring v) (cons 'd v))
(define value '(a b))

(check-equal? (compile-message
               (declaring value)
               #f)
```

The doctor's plan for that file is:

```text
E1 [low] insert `)` at 10:19 -- append after the last code token
    -                #f)
    +                #f))
SIMULATION: after E1..E1 the file lexes BALANCED (weakest edit: low)
```

Applying the plan yields this file, which differs from the one above only in where the closer
sits:

```racket
#lang racket/base

(define (compile-message s) (list s))
(define (check-equal? a b) (cons a b))
(define (declaring v) (cons 'd v))
(define value '(a b))

(check-equal? (compile-message
               (declaring value)
               #f))
```

That lexes `BALANCED` and raises `compile-message: arity mismatch; expected: 1, given: 2`. The
intended repair adds the closer to the `declaring` line instead, and that file runs. With no
indentation evidence here the doctor's tier is `[low]` and its plan appends after the last code
token; the question it asks offers `B: some end earlier` as the default, so an agent that follows
the plan rather than the question applies the wrong repair.

The indentation evidence itself was accurate: in the earlier round it correctly identified a
missing test-case closer and its column, and it gave no false positives on the files that
compiled. The gap is placement, not detection.

## Proposed fix

- Validate a plan against a parse, not a delimiter tally. After applying each candidate
  insertion, read the repaired text and report which form each closer closes, e.g. `the closer
  at 339:56 closes compile-message, leaving #f outside it`. Reject a placement that makes a
  constant or variable an argument of a form that already has arguments.
- Derive the insertion column from the parse tree instead of the token stream: the closer
  belongs at the end of the last line that holds the *final element* of the form being closed,
  which is not the last code token of the previous line when that token is an argument of an
  outer form.
- Mark an append-at-end-of-file plan as unverified whenever the enclosing form has arguments on
  lines indented deeper than its opener, since that shape means the form ends mid-file.
- Optionally add a `--verify` mode that runs Racket's reader over the repaired text in a
  temporary file and treats a read error as a failed plan. The check costs one reader pass per
  plan, and the compiler's own message names the form the plan broke.
