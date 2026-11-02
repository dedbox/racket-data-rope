# The Rope Library

Fast, pure-Racket generalized rope: multi-lib package `data-rope`, Racket v9.3 [cs] (Chez backend).
Optimize response-time latency and a low-friction API. Read `racket-facts.md` first: it records hard
lessons that save time.

## Layout

- Primary sources `data/rope/` and `data/rope.rkt` · docs `data/rope/scribblings/` · tests `tests/` ·
  benchmarks `benchmarks/` · design notes `notes/*.org` · checks `tools/lint.sh`

## Commands

```sh
raco setup --pkgs data-rope                             # sources + metadata + docs
raco setup --no-info-domain --no-docs --pkgs data-rope  # sources only
raco make data/rope/some-file.rkt                       # one source file
raco test tests/                                        # all tests
raco test tests/X.rkt                                   # one test file
raco cover -p -b data-rope                              # report -> coverage/
racket benchmarks/latency-gate.rkt                      # latency gate vs notes/m0.org §3.2 row 12 ceilings
```

Compile the sources and the files about to run before every test or benchmark run: a stale `.zo`
silently runs old code (`racket-facts.md` §5). Micro-benchmarks backing an architectural or design
decision stay in `benchmarks/`. The latency gate is a benchmark, not a test: `raco test tests/`
never runs it (`ADR-026`).

## Test files

rackunit hierarchy: `test-suite` definitions with `test-case`s at the leaves.

- Unit tests for `data/rope/A.rkt` -> `tests/A-tests.rkt`: the whole file, `require` statements
  included, sits inside one top-level `(module+ test ..)`.
- Integration tests for `data/rope/A.rkt` and `data/rope/B.rkt` -> `tests/A--B-tests.rkt`.

## Code style

- Modern `#lang racket`: clean, macro-safe, functional, idiomatic. No legacy PLT Scheme or
  generic-Lisp code.
- Macros use `define-syntax-parse-rule` (`syntax/parse`), never manual analysis. Require with
  `(prefix-in x/ …)`, not `(prefix …)`.
- `match` sparingly. No `set!` unless it measurably wins and is encapsulated safely; `parameterize`
  is ok.

## Comments

Three tiers: module header, section header, definition doc. They govern Racket comments and
`notes/*.org` under `data/rope/`, `tests/`, and `benchmarks/`; markdown is exempt, and
`benchmarks/dev-shim/` (a mirror of the oracle library) is exempt.

### Dividers

- Module header: `;;; path -- one-line role.`, blank `;;;`, then `=== Name ===` blocks
  (`=== What a rope is ===`, `=== Naming conventions ===`, `=== Key invariants ===`). ≤ 24 prose
  lines; blank separators and `===` headings do not count.
- Section rule: an 80-column `;; ---...---`, title on the next line, optional ` -- gloss` naming the
  section's character (`-- standard AVL rotations`, `-- O(log n) path-copy operations`,
  `-- diagnostics only`). Text under the rule belongs to the section, never to a function.
- A bare `;;` is a paragraph break: one idea per paragraph, 2-4 lines.
- Test files use `;; ===...===` rules over `test-suite` groups.

### Definition docs

- Directly above what they describe, never between two definitions.
- Imperative, result-first: `Return the elements in [i, j).` not `Returns ...`. Put contract edge
  cases in the doc (`When `i = j`, return the empty rope`); keep implementation narration out -- the
  body below already shows the steps.
- Struct fields sit above the struct as aligned `name -- gloss` columns: operation fields use
  signature notation (`slice -- chunk nat nat → chunk`), data fields a sentence gloss ending in a
  period.
- Self-evident one-liners get no doc (`rope?`, `first-leaf`); the section header carries them.
- ASCII diagrams sit beside the function they explain, led by one line naming the outcome:
  `Left rotation: the right child becomes the new root.`. Node placeholders stay `ρ`, `ρ′`, `ρ′′`,
  `ρ*`, `·₀`/`·₁`/`·₂`.
- `Precondition:` is the last line above a definition (`height(l) > height(r) + 1 ⇒ l is a node`).

### Never in source

- No roadmap or layer talk: no unwritten layers (types, ops, classes), no architecture tables. Those
  live in `notes/*.org`.
- No speculative maintenance advice ("keep it that way", "in case the representation changes"). State
  the invariant instead.
- No measured figures: numbers live in `notes/m0.org` §3.2 and `racket-facts.md`; the source keeps the
  decision and the rule, then cites (`See notes/m0.org §3.2 row 8.`). Re-pinning a ceiling touches the
  notes and one pointer, never a second copy of the prose.
- No fact stated twice: a section-wide fact appears in the section header only. Keep every ADR id and
  notes row that constrains the code -- delete prose, never pointers.

### Prose

- Never `land` or `landed` for work or changes arriving: say `add` / `added` or `include` /
  `included` -- `the gate is added to benchmarks/`, never "the gate lands in benchmarks/".
  The rule covers comments, notes, and commit messages alike. Positional uses stay: an edit
  landing inside a leaf names a location, not a delivery.
- Never `frontier` for a configuration a sweep settles on: say `balance point`, or name the
  costs it trades -- `so 512/64 is the balance point`, never "the frontier point". The rule
  covers comments, notes, and commit messages alike.
- Fill the column: prose lines sit in the 85-91 band, hard cap 93, ragged only on a paragraph's last
  line. Paragraph breaks fall between lines, never mid-sentence.
- Lists use `-`: pack single-line items with no blank lines between them, separate with a blank line
  only when more than one item wraps to a second line, and align continuations to the item's text
  column (the comment prefix plus 3 spaces).
- Backtick identifiers, never paraphrase: `the last leaf of `l`` beats "the last leaf of the left
  rope". Comment names match the code's (`l`, `r`, `ρ′`).
- Bold (`**runtime**`) marks a term being defined, at most once per module; no italics.
- Cite papers as `Blelloch, Ferizovic & Sun (2016)` -- authors and year, no title. Project evidence as
  `ADR-019`, `notes/m0.org §3.2 row 8`, `benchmarks/limit-sweep.rkt`; keep the `.org` suffix and the
  `e.g.,` / `i.e.,` forms.
- Hedge claims to the regime measured (`at 10 MB`, `for files at this size`); never `always`, never
  `the biggest win in this file`.
- Proofread the rewrite in the same pass, then run `tools/lint.sh`: it checks trailing whitespace,
  `λ` over `lambda`, imperative mood, `-` lists, note citations, and the 93-column cap.

## Typography

`λ` not `lambda`; prose keeps ASCII `->`, never unicode arrows; no LaTeX unless a formula cannot be
expressed in Unicode; Greek letters (`τ`, `σ`, `ρ`, `α`) are fine. Comments may use `≤` and `≥`; code
keeps `<=` and `>=`. `→` appears only inside notation lines (`slice -- chunk nat nat → chunk`) and `⇒`
only in `Precondition:` lines.

## Design notes

Design notes are `notes/*.org`; `racket-facts.md` stays at the root.

### File names

`notes/<key>[-<topic>].org`: lowercase, words joined by `-`, no `-notes` suffix (the directory already
names the kind). The keys are a closed set; adding a key is a decision, not a habit.

| Key | Example | Holds |
|-----|---------|-------|
| `overview` | `overview.org` | the design in one page: layers, decisions worth review, no measurements |
| `m<N>` | `m0.org`, `m1.org` | one milestone's record: scope, decisions, exit gates, measured ceilings |
| `adr-<NNN>` | `adr-019.org` | a decision that outgrew its row in a milestone table |
| `bench-<topic>` | `bench-avl-vs-treap.org` | a measurement write-up that outlives the milestone that ran it |
| `spike-<topic>` | `spike-typing.org` | a throwaway investigation; folded into a milestone file, then deleted |

- One idea per file. A milestone record that outgrows its sections splits as `m<N>-<topic>.org`
  (`m2-typing.org`), carrying the section numbering with the text it moves.
- Section numbers (§3.2) and table row numbers are permanent: re-point a citation when text moves
  between files, never renumber a target another file cites.
- A note is created in the same pass as the decision it records, and cited from source by full path
  (`### Prose` gives the form). Nothing else lives here: no markdown, no index, no transcripts. The key
  scheme is the catalogue.

### Markup

- Headings: one `*` per depth level, never skipping a level. First line of the file is `#+title:`,
  plus `#+subtitle:` when useful.
- `#+date:` records the last decision pass, and a milestone file carries it. Update it whenever the
  file gains or signs off a decision, so a stale gate shows without reading the body.
- Code listings go in `#+begin_src racket … #+end_src`. Reserve `#+begin_src text` for non-source
  content such as an inference grammar or a compile-time error message.
- Inline code: `=chunk-dict=` for names with no literal `=`; use `~` for quotes (`~elem<?~`) and for
  names holding a literal `=` with no spaces either side (`~rope=?~`).
- Never a spaced `=` inside a delimiter pair: write plain text (`merge-limit = limit/8`) or split the
  names (`=merge-limit=` = `=limit/8=`). Never an apostrophe possessive inside one (`core.rkt='s` is
  invalid): write "the pair of structs in `=core.rkt=`".
- Bold `*text*`, italic `/text/`, kept rare; prefer code delimiters for identifiers and match the
  density the existing notes use.
- Tables: `+` separator row, as in `|---+---|`; no `|` inside a cell; inline-code rules apply inside
  cells.
- Legend table of review state (`Given` / `Accepted` / `Conditional` / `Proposed` / `Provisional`);
  mark unverified claims `⟦verify⟧`.

## Operational constraints

- Edit nothing outside the project directory (`/` is not writable); temporary files go in
  `/tmp/opencode/`. Ask before installing external libraries.
- Never `git push`, `git rebase`, `git merge`, or delete existing branches.
- Answer concisely, code snippets over paragraphs. Consult `/usr/share/doc/racket/reference/` rather
  than guessing.
- You run on a tightly quantized local model: use square brackets `[]` wherever Racket idiom expects
  them, so nested grammar blocks stay unambiguous.

## Git commit messages

Subject: first line, under 68 characters, imperative, lowercase. Then a blank line, then the body (no
length limit; wrap ~72).

### Merge-squash commits into `dev`

A merge-squash of a feature branch into `dev` replaces a whole branch of history with one commit, so
its message must stand on its own: a reader who never saw the branch or the design notes still
understands what changed.

- Content: the achievements as list items, then what each one entailed, in concrete terms. No
  milestone names and no citations into `notes/*.org` or decision records -- the message restates the
  outcome rather than pointing at the record.
- Files and figures: name the files that changed and the values the measurements settled -- a chunk
  limit, a bytes-per-element figure, a latency ratio -- never the internal labels for them.
- Plain words: no `pinned`, `gated`, `ceilings`, or `close-out`; `add` / `include` over `land`
  (§Prose). Do not assume the reader knows why a knob matters.
- State what is absent: if the commit ships no public API or no documentation, say so and name what
  comes next.
- Test results: a line for the latency gate or the suite is allowed only when the run happened before
  the merge, and it names the closest margin instead of asserting a vague pass.
- Formatting: the rules above still hold, with `*` list items whose continuations indent two spaces
  and no body line past 72 columns.
