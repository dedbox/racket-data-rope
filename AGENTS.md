# The Rope Library

Fast, pure-Racket generalized rope. Multi-lib package `data-rope`, Racket v9.3 [cs] (Chez Scheme
backend). Optimize for response-time latency and a low-friction API.

Read `racket-facts.md` first: it records hard lessons learned that save time.

## Layout

* Primary sources: `data/rope/`, `data/rope.rkt`
* Docs: `data/rope/scribblings/` · Tests: `tests/` · Benchmarks: `benchmarks/`

## Commands

```sh
raco setup --pkgs data-rope                             # sources + metadata + docs
raco setup --no-info-domain --no-docs --pkgs data-rope  # sources only
raco make data/rope/some-file.rkt                       # one source file
raco test tests/                                        # all tests
raco test tests/X.rkt                                   # one test file
raco cover -p -b data-rope                              # report -> coverage/
```

Compile the primary sources and the files you are about to run before every test or benchmark run:
a stale `.zo` silently runs old code (`racket-facts.md` §5). Keep every micro-benchmark used for an
architectural or design decision in `benchmarks/`.

## Test files

rackunit hierarchy: `test-suite` definitions with `test-case`s at the leaves.

* Unit tests for `data/rope/A.rkt` -> `tests/A-tests.rkt`; everything in that file, including
  `require` statements, sits inside a top-level `(module+ test ..)`.
* Integration tests for `data/rope/A.rkt` and `data/rope/B.rkt` -> `tests/A--B-tests.rkt`.

## Code style

* Modern `#lang racket`: clean, macro-safe, functional, idiomatic. No legacy PLT Scheme or
  generic-Lisp code.
* Macros use `define-syntax-parse-rule` (`syntax/parse`), never manual analysis. Require with
  `(prefix-in x/ …)`, not `(prefix …)`.
* `match` sparingly. No `set!` unless it measurably wins and is encapsulated safely; dynamically
  scoped parameters (`parameterize`) are ok.

## Typography

Applies to Racket source and `*.org` alike: `λ` not `lambda`; ASCII `->`, never unicode arrows; no
LaTeX unless a formula cannot be expressed in Unicode; Greek letters (`τ`, `σ`, `ρ`, `α`) are fine.

## Org-mode markup

Design notes are `*.org` at the project root (`redesign-overview.org`, `M0-notes.org`,
`M1-notes.org`). These rules apply to those files only, not to markdown.

* Headings: one `*` per depth level, never skipping a level. First line of the file is `#+title:`,
  plus `#+subtitle:` when useful.
* Code listings go in `#+begin_src racket … #+end_src`. Reserve `#+begin_src text` for non-source
  content such as an inference grammar or a compile-time error message.
* Inline code: `=chunk-dict=` for names with no literal `=`; use `~` for quotes (`~elem<?~`) and for
  names containing a literal `=` with no spaces on either side (`~rope=?~`).
* Never a spaced `=` inside a delimiter pair: write plain text (`merge-limit = limit/8`) or split
  the names (`=merge-limit=` = `=limit/8=`). Never an apostrophe possessive inside one
  (`core.rkt='s` is invalid) — write "the pair of structs in `=core.rkt=`".
* Bold `*text*`, italic `/text/`, kept rare; prefer code delimiters for identifiers and match the
  density the existing notes use.
* Tables: `+` separator row, as in `|---+---|`; no `|` inside a cell; inline-code rules apply inside
  cells.
* Legend table of review state (`Given` / `Accepted` / `Conditional` / `Proposed` / `Provisional`);
  mark unverified claims `⟦verify⟧`.

## Operational constraints

* Edit nothing outside the project directory (`/` is not writable); put temporary files in
  `/tmp/opencode/`. Ask before installing external libraries.
* Never `git push`, `git rebase`, `git merge`, or delete existing branches.
* Answer concisely, code snippets over paragraphs. Consult `/usr/share/doc/racket/reference/` rather
  than guessing.
* You run on a tightly quantized local model: use square brackets `[]` wherever Racket idiom expects
  them, so nested grammar blocks stay unambiguous.

## Git commit messages

Subject: first line, under 68 characters, imperative, lowercase. Then a blank line, then the body
(no length limit; wrap ~72 for legibility).
