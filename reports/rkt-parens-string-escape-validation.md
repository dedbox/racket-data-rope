# `rkt_parens` accepts invalid string escapes

**Type:** feature request
**Component:** `rkt_parens`, the delimiter doctor for Racket source files
**Severity:** the `BALANCED` verdict is wrong for a file the reader rejects

## Summary

The doctor scans string tokens to keep quotes out of the delimiter tally, but it does not check
what is inside them. A string containing a backslash the reader does not recognize is a read
error, and the doctor still reports `BALANCED`. That verdict is what makes the doctor worth
consulting before an edit, so an escape error slips through a check that is supposed to say
"this file parses".

## Evidence

This file is delimiter-clean and the doctor agrees, while the reader refuses it:

```racket
#lang racket/base

;; a delimiter-clean file whose regexp literal holds an escape the reader rejects.
;; The doctor reports BALANCED; the reader names the escape.
(define rx #rx"pair\?: two arguments")

(displayln (regexp-match rx "pair?: two arguments"))
```

Doctor: `escape.rkt: BALANCED`, exit 0. Reader:

```text
escape.rkt:5:11: read-syntax: unknown escape sequence `\?` in string
```

Byte strings behave the same way, with their own message: a `#rx#"pair\?: ..."` literal gets
`unknown escape sequence `\?` in byte string`.

In the session that surfaced this, a doctor-clean edit to a test file introduced
`#rx"pair\?: ..."`; the failure showed up only when the file was compiled, so the doctor's
verdict cost a compile round trip it was meant to prevent.

The rest of the token scan is sound. A file containing `#")"` (a character literal for a
double quote), `#"bytes (unclosed"` (a byte string holding an unmatched opener), `#rx#"..."`,
and a `#<<` here string whose body holds a `"` and a `(` reports `BALANCED` and parses, so
escapes are the missing piece, not token recognition.

## The rule the reader applies

Racket's reader reference (`reader.html` §1.3.7) gives the escapes recognized inside a string:
`\a`, `\b`, `\t`, `\n`, `\v`, `\f`, `\r`, `\e`, `\"`, `\'`, `\\`, octal `\0`-`\7` (one to three
digits, the value must be at most 255), `\x` (one or two hex digits), `\u` (up to four hex
digits, and `\uXXXX\uXXXX` for a surrogate pair), `\U` (up to eight hex digits), and a
backslash followed by a newline, which is elided. Any other use of a backslash raises
`exn:fail:read`. Byte-string constants accept the same set except `\u` and `\U`. Confirmed by
reading each form: `\p`, `\q`, `\s`, `\8`, `\xZZ`, and `\uZZZZ` all fail; `\e`, `\'`, `\1`,
`\x41`, `\u0041`, and `\U0001F600` all succeed.

## Proposed fix

- Add an escape pass over every string, byte-string, and regexp literal token the scan already
  recognizes, and report the reader's own wording: `escape.rkt:5:11: unknown escape sequence
  \? in string`, with a separate message for byte strings.
- Treat a backslash at end-of-line as valid (the elided-newline convention), so line
  continuation is not flagged.
- Skip here-string bodies: `#<<` recognizes no escape sequences, so a backslash there is content.
- Emit these findings alongside delimiter errors, with the same exit status, so a caller that
  trusts exit 0 gets a file the reader accepts.
