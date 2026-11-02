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

## Comments

Three tiers: module header, section header, definition doc. Applies to `data/rope/`, `tests/`, and
`benchmarks/`; `benchmarks/dev-shim/` mirrors the oracle library and is exempt. The rules cover
Racket comments and `*.org` notes, not markdown.

### Dividers

* Module header: `;;; path -- one-line role.`, blank `;;;`, then `=== Name ===` blocks
  (`=== What a rope is ===`, `=== Naming conventions ===`, `=== Key invariants ===`). ≤ 24 prose
  lines; blank separators and `===` headings do not count.
* Section rule: an 80-column `;; ---...---`, title on the next line, optional ` -- gloss` naming
  the section's character (`-- standard AVL rotations`, `-- O(log n) path-copy operations`,
  `-- diagnostics only`). Text under the rule belongs to the section, never to a function.
* A bare `;;` is a paragraph break. One idea per paragraph, 2-4 lines.
* Test files use `;; ===...===` rules over `test-suite` groups.

### Definition docs

* Directly above what it describes, never between two definitions.
* Imperative, result-first: `Return the elements in [i, j).` not `Returns ...`.
* Contract edge cases go in the doc (`When `i = j`, return the empty rope`); implementation
  narration stays out -- the body below already shows the steps.
* Struct fields listed above the struct as aligned `name -- gloss` columns. Operation fields use
  signature notation (`slice -- chunk nat nat → chunk`); data fields use sentence glosses ending
  in a period.
* Self-evident one-liners get no doc (`rope?`, `first-leaf`); the section header carries them.
* ASCII diagrams sit beside the function they explain, led by one line naming the outcome
  (`Left rotation: the right child becomes the new root.`). Node placeholders stay `ρ`, `ρ′`,
  `ρ′′`, `ρ*`, `·₀`/`·₁`/`·₂`.
* `Precondition:` is the last line above a definition (`height(l) > height(r) + 1 ⇒ l is a node`).

### Never in source

* Roadmap and layer talk: no unwritten layers (types, ops, classes), no architecture tables. Those
  live in the `*.org` notes.
* Speculative maintenance advice ("keep it that way", "in case the representation changes"). State
  the invariant instead.
* Measured figures. Numbers live in `M0-notes.org` §3.2 and `racket-facts.md`; the source keeps the
  decision and the rule, then cites (`See M0-notes.org §3.2 row 8.`). Re-pinning a ceiling must
  touch the notes and one pointer, not three files of prose.
* Any fact stated twice: a section-wide fact appears in the section header only. Keep every ADR id
  and notes row that constrains the code -- delete prose, never pointers.

### Prose

* Fill the column: prose lines land in the 85-91 band, hard cap 93, ragged only on a paragraph's
  last line. Paragraph breaks fall between lines, never mid-sentence.
* Lists use `-`. Pack single-line items with no blank lines between them; separate with a blank line
  only when more than one item wraps to a second line. Continuations align to the item's text column
  (the comment prefix plus 3 spaces).
* Backtick identifiers, never paraphrase: `the last leaf of `l`` beats "the last leaf of the left
  rope". Comment names match the code's names (`l`, `r`, `ρ′`).
* Bold (`**runtime**`) marks a term being defined, at most once per module; no italics.
* Cite papers as `Blelloch, Ferizovic & Sun (2016)` -- authors and year, no title. Project evidence
  as `ADR-019`, `M0-notes.org §3.2 row 8`, `benchmarks/limit-sweep.rkt`; keep the `.org` suffix and
  the `e.g.,` / `i.e.,` forms.
* Hedge claims to the regime measured (`at 10 MB`, `for files at this size`); never `always`, never
  `the biggest win in this file`.
* Proofread the rewrite in the same pass. Self-check:

  ```sh
  grep -rn --include='*.rkt' '[[:space:]]$' data/rope tests benchmarks     # trailing whitespace
  grep -rn --include='*.rkt' '(lambda (' data/rope tests benchmarks        # typography: λ
  grep -rn --include='*.rkt' -E '^ *(;;;?)[[:space:]]+(Returns|Builds|Concatenates|Replaces)' \
    data/rope tests benchmarks                                            # imperative mood
  grep -rn --include='*.rkt' -E '^ *(;;;?)[[:space:]]+\* ' data/rope tests benchmarks  # lists use -
  grep -rn --include='*.rkt' 'M0-notes[^.o]' data/rope tests benchmarks    # cite the .org file
  awk '/^ *(;;;?)/ && length($0) > 93' data/rope/*.rkt tests/*.rkt benchmarks/*.rkt
  ```

## Typography

Applies to Racket source and `*.org` alike: `λ` not `lambda`; ASCII `->`, never unicode arrows; no
LaTeX unless a formula cannot be expressed in Unicode; Greek letters (`τ`, `σ`, `ρ`, `α`) are fine.
Comments may use `≤` and `≥` (they are typography like Greek letters); code keeps `<=` and `>=`. `→`
appears only inside notation lines (`slice -- chunk nat nat → chunk`) and `⇒` only in
`Precondition:` lines; prose keeps ASCII `->`.

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
