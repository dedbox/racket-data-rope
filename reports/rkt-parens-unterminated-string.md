# `rkt_parens` names the last unpaired quote, not the stray one

**Type:** bug report plus feature request
**Component:** `rkt_parens`, the delimiter doctor for Racket source files
**Severity:** the primary diagnostic points away from the defect, and the repair plan it
proposes does not fix the file

## Summary

When a string literal is missing its closing `"`, the reader absorbs everything after the
open quote into one multi-line string, so the string that looks broken is the last one in the
file. The doctor reports that last quote, and its repair plan appends bracket closers at
end-of-file. The stray quote is the *first* unescaped `"` whose string spans lines, and that
is the site the repair belongs at.

Racket allows bare newlines inside a string literal, so nothing in either the doctor's report
or the reader marks the multi-line string as suspicious. Treating such a string as a smell is
what makes the defect visible in source, not just at read time.

## Evidence

Editing a throwaway copy of a rackunit test file removes the closing quote from a string inside a
test case, so line 214 ends `... "abcdef))))`. The doctor's first line names that copy; the name
is replaced below with `<the copy under test>`, because the handoff needs no access to that tree:

```text
<the copy under test>:438:22: error: unterminated string starting here -- everything after it is swallowed
  ORIGIN: the construct itself starts at 438:22
  HINT: multi-line strings in this file (a stray `"` often creates one): 215:78->line 219, 219:37->line 222,
        224:71->line 228, 228:43->line 238, 239:81->line 244
```

Racket's reader agrees with the same line (`438:21: read-syntax: expected a closing "`),
because both pair quotes greedily from the front. The repair plan then proposes:

```text
E1 [low] insert `)))))))` at 443:24 -- append after the last code token
SIMULATION: still broken after the planned edits
```

The two candidate repairs, measured against the same file:

| Repair site | Doctor verdict | Test run |
|-------------|----------------|----------|
| quote added at 214 (the intended closer) | `BALANCED` | 31 tests pass |
| quote added at 438 (the site the report names) | 4 forms still unclosed | fails to compile |

Closing the last quote does not restore the balance, because the swallowed region is where the
missing closers live. The hint already lists the multi-line strings, and the first of them
starts at `215:78`, immediately after the stray quote -- the strongest signal in the report,
and it is not the answer the question offers.

The question block reflects the same skew: `Q2 [line 438]` asks what is intended at the site
the report names, and its option C ("an earlier string/comment ended too early -- look for an
unescaped `"`") is the right branch but names no location to look at.

## Minimal reproduction

```racket
#lang racket/base

(define (assert x msg)
  (check-exn msg (λ () (run "abcdef
    (check-exn "other" (λ () (void 1)))
    (displayln "done")))
  (void x))

(define (run s) s)
(define (check-exn m thunk) (thunk))

(assert 1 "first")
```

A quote pairing scan that starts at the front pairs `"abcdef` with the quote opening `"other"`,
pairs `"other"`'s closer with `"done"`'s opener, and leaves `"done"`'s closer running to EOF.
The doctor calls out that last quote; the quote actually missing is the one that opens the string
on line 4 above.

## Feature request: flag multi-line strings as a smell

Racket accepts a bare newline inside a string, and `\<newline>` is a valid escape that elides
it, so both forms are legal. Neither is a delimiter problem, and both hide a stray quote. A
`SMELL` section that lists string literals spanning more than one source line would make the
stray quote visible in a file that otherwise reads cleanly.

Measured cost of such a warning: zero. The tree where the doctor was run holds library sources,
test files, and benchmark files, and none of them holds a string literal spanning lines or an
unterminated quote, so the rule would report nothing there today. Exempt from the warning:
here-strings (`#<<`) and `\<newline>` continuations, which are intentional multi-line forms.

## Proposed fix

- Report the earliest candidate site for an unterminated string: the first quote that opens a
  multi-line string inside a cascade, and pass that location into `Q2` option C rather than
  telling the reader to look for an unescaped `"` without saying where.
- Keep the last-quote line as secondary output, since it is where the reader complains.
- Add the `SMELL` section described above, emitted even when the file lexes `BALANCED`.
- Drop the append-at-EOF proposal for a file whose first surface error is an unterminated
  string: no bracket plan can fix a swallowed region, so the plan should say so instead of
  simulating closers that the simulation already rejects.
