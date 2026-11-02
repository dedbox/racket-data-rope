# Racket facts learned in the limit-sweep session (Racket v9.3 [cs])

Read-me-first: these are facts about the *runtime and toolchain*, not about the rope library.
Each entry is marked VERIFIED (measured this session) or ASSUMED (inference, re-check before use).

## 1. Timing: use this clock, not that one

| Fact | Value | Status |
|---|---|---|
| `current-inexact-monotonic-milliseconds` exists in `racket/base` | yes | VERIFIED |
| Its resolution | ~0.25 µs (measured minimum delta 0.000253 ms) | VERIFIED |
| Its magnitude | small (counts from process boot: fresh process reads ~98.2) | VERIFIED |
| Why it beats `current-inexact-milliseconds` | resolution ~0.73 µs; magnitude is an epoch, so the flonum's ulp degrades | VERIFIED |
| Per-operation resolution when sampling a window | resolution ÷ ops in window (0.25 µs / 32 ops ≈ 8 ns) | VERIFIED |
| Meaning of a ~20 ns measured mean | near the measurement floor; treat as clock-bound, not algorithm-bound | VERIFIED |
| Window size rule | a window must last several ticks: 32 ops at ~1 µs = ~32 µs, fine; 32 ops at ~20 ns = ~0.7 µs, marginal | VERIFIED |
| Absolute ns while another process competes for cores | inflated on the edit scenarios: bytes typing p50 648 ns against 481 quiet at 100 K, clean p99 1484 against 817; `randref` moves 1-3%; ratios between scenarios inside one process stay comparable | VERIFIED (2026-11-02) |

Recipe, for hot ops:

```racket
(define (now-ms) (current-inexact-monotonic-milliseconds))   ; ns resolution: multiply by 1e6
(define (gc-ms) (current-gc-milliseconds))                   ; flag samples that straddled a GC
```

Percentiles: report p50/p99/p99.9/max over the samples, then **again** over only the samples that
did not straddle a collection. Sorting/ranking by the no-GC p99 (with p50 as tie-breaker) gives the
algorithm's own tail; the all-sample p99 mixes in GC. Both belong in the report.

## 2. Memory measurement: what works and what does not

| Metric | Verdict | Note |
|---|---|---|
| `current-memory-use` deltas around a copy | **accurate for string/bytes copies** | it counts bytes of those objects |
| `current-memory-use` deltas around struct/pair allocation | **undercounts** | not byte-exact for cons/structs |
| `/proc/self/status` `VmRSS:` deltas over a live rope | **unreliable** | CS reuses heap pages, so deltas often read 0 |
| Struct sizes (calibrated by RSS over 4M structs) | leaf (3 fields) ≈ 32 B, node (5 fields) ≈ 48 B | raw readings 31.9 / 58.2 B: page granularity |
| Chunk payload | string ≈ **4 bytes/char** (CS backend); bytes = **1 byte/byte** | VERIFIED |
| Retained bytes per element, measured shape | string rope 4.156 @ limit 512; bytes rope 1.039 @ limit 2048 | tree adds < 0.2 B/elem; payload dominates |
| Tree overhead formula | `leaves × 32 + (leaves - 1) × 48` (nodes = leaves - 1) | use this, not RSS |

Reading procfs correctly:

```racket
(define (rss-kb)
  (define s (file->bytes "/proc/self/status"))          ; `file->lines` truncates procfs
  (define m (regexp-match #"VmRSS:[ \t]*([0-9]+)" s))   ; digits only, the " kB" suffix is noise
  (and m (string->number (bytes->string/utf-8 (cadr m)))))
```

Memory pressure of a stress run: a 100 MB **string** rope is ~400 MB payload (+ tree); a 100 MB
**bytes** rope is ~100 MB. Keep string stress ≤ 10 MB chars on this machine (~1 GB free RAM).

## 3. Bindings that differ from the docs/guesses

| Thing | Reality | Status |
|---|---|---|
| `bytes/elem`, `bytes-per-elem`, any name containing `/` | a *procedure* (division operator) if unresolved; `(define bytes/elem …)` shadows it | VERIFIED |
| `round` / `max` in a `define-values` list | shadows the builtin and produces `application: not a procedure` | VERIFIED |
| `bytes->number` | does not exist in this install | VERIFIED |
| `bytes->string/utf8` | does not exist; use `bytes->string/utf-8` | VERIFIED |
| `for/string` | does not exist | ASSUMED |
| `exact-floor` | needs `(require racket/math)` | VERIFIED |
| `command-line` | needs `(require racket/cmdline)` + `(module+ main …)` | VERIFIED |
| `command-line #:args` values | arrive as **strings**, not bytes; converting them with `string->bytes/utf-8` breaks `string-split` | VERIFIED (2026-11-02, fixing `benchmarks/latency-gate.rkt`) |
| `string-join` | `racket/string` | VERIFIED |
| `~a` formatting | `racket/format` | VERIFIED |
| `exact->inexact` | available from `racket/base` | VERIFIED |

## 4. Module/require mechanics (bit twice, wasted ~30 min each)

- A module with `#lang racket/base` and **no `provide`** exports **nothing**. `(require (only-in "mod.rkt" id))` fails with `only-in: identifier not included in nested require spec`. Benchmark/debug scripts cannot pull internals from a private module; write the probe inside that module or use its `submod`.
- `(require "/absolute/path.rkt")` is a **bad module path**. Relative paths resolve relative to the *requiring file*, so a probe file must live next to the module it requires (e.g. `benchmarks/-probe.rkt`, then delete it).
- `#lang racket/base` module + `(module+ main …)` means `racket file.rkt` runs main, and `require` of that module runs only top-level definitions.

## 5. Compilation: the stale-.zo trap

`raco make` writes `dir/compiled/*_rkt.zo`, and Racket loads it when it is newer than the source.
A **stale `.zo` silently runs old code** — it can drop output or throw errors whose line numbers
match the old source, which is confusing. Always:

```sh
raco make benchmarks/limit-harness.rkt benchmarks/limit-sweep.rkt   # before every run
rm -rf benchmarks/compiled && raco make …                            # when in doubt
```

## 6. Toolchain behavior verified here

The commands are listed in `AGENTS.md` §Commands. Behavior measured on this machine: the full test
suite is ~114 tests and prints per-file summary lines; docs setup also writes man pages plus
`data/rope/doc/`; coverage output lands in `coverage/`. The latency gate prints one line per cell and
exits 1 on a breach; it needs a quiet box, where a run costs ~1 min per shape.

## 7. Shell gotchas observed

- Default foreground timeout is 120 s: long sweeps must go `background=true` with `timeout: 0`.
- `for id in …; do …; done` inside a quoted one-liner needs `bash -c` with correct quoting; plain
  heredoc-less quoting produced `syntax error: unexpected end of file from 'for'`.
- Don't chain `echo "===="` separators into output; it makes the user-visible transcript worse.

## 8. Editing Racket source safely

- `read`-output line-number prefixes are not part of the file; strip them in `oldString`.
- Python-driven edits of paren-heavy Racket are the failure mode: an off-by-one close silently
  breaks a *later* form ("expected a `)` to close `(`, possible cause: indentation suggests a
  missing `)` before line N). Verify balance after each edit:
  ```python
  def bal(line, d):
      for ch in line:
          if ch in '([': d += 1
          elif ch in ')]': d -= 1
      return d
  d = 0
  for ln in open('file.rkt'): d = bal(ln, d)
  print('final depth:', d)      # must be 0
  ```
- `application: not a procedure; given X` shows the **callee** value, not the argument. A flonum in
  "given" means a shadowed builtin (`max`, `round`, an identifier with `/`).

## 9. Library/runtime facts measured for benchmarking (rope core, `data/rope/core.rkt`)

- `chunk->rope` reuses the source chunk verbatim when its length ≤ `limit`; otherwise splits at
  multiples of `limit`; balanced binary tree ⇒ nodes = leaves − 1, height ≈ log2(leaves).
- `rope-concat` merges the two boundary leaves only when their combined length ≤ `merge-limit`;
  a merged leaf is bounded by `merge-limit`, so the invariant `merge-limit ≤ limit` is what keeps
  `rope-valid?` true.
- `rope-splice` replaces `[start, end)`; an insert lands inside a leaf and copies ≤ `limit` per
  touched leaf; boundary merging decides copies per edit.
- Shape reference points: 10M chars @ limit 512 → 19532 leaves, height 15; 100M bytes @ limit 2048
  → 48829 leaves, height 16.

Latency reference (ns, 10 MB buffer, p50 / clean p99 — a 20-window run; the bytes shape was
re-measured in 2026-10-31 below, which reports higher p50 and tails on the mutating scenarios):

| scenario | string 512/64 | string 512/512 | bytes 2048/512 | bytes 2048/2048 |
|---|---|---|---|---|
| 1-char insert | 536/822 | 1251/2317 | 363/596 | 681/893 |
| 4-for-4 patch | 1632/2504 | 1963/12658 | 1080/1258 | 1305/8670 |
| delete 1 | 1442/1646 | 1832/16894 | 806/954 | 1111/11043 |
| paste 30 chars | 1247/1596 | 1205/20996 | 910/1042 | 869/993 |
| random `rope-ref` | ~203 | ~180 | ~138 | ~135 |
| token slice | ~21 (floor) | ~21 | ~21 | ~21 |
| chunk scan 50 KB | ~1922 | ~1702 | 828 | 896 |
| 1000 sequential refs | ~26885 | ~25596 | ~22992 | — |
| flatten whole buffer | ~27 ms (insensitive) | — | ~6 ms (insensitive) | — |
| load 10 MB | ~216 ms (insensitive) | — | ~272 ms (insensitive) | — |

Re-measured 2026-10-31 at 1024 windows/scenario, six runs of the bytes shape (medians, ns,
p50 / clean p99): typing `486/928` at 100 K elements and `399/1008` at 10 MB for `bytes/2048/256`,
against `483/1332` and `396/1036` for `bytes/2048/512`; random `rope-ref` 24 at 100 K and 88 at
10 MB (clean p99 41 and 163); token slice ~21 ns at either size. Patches, deletes and pastes land
within each other's tail noise, while merging up to `limit` costs 6.9–8.5 µs clean p99 on those
shapes at 10 MB. Shape over the runs: 49 leaves at height 6 (100 K), 4883 leaves at height 13
(10 MB), ~1.04 B/element.

Same six-run treatment for the string shape on 2026-11-01, `string/512/64`: typing `544/1079.5`
at 100 K elements and `515.5/1150.5` at 10 MB, random `rope-ref` `29/80.5` and `139/219`, 196
leaves at height 8 and 19532 at height 15, ~4.16 B/element. `limit 256` types ~11% cheaper but
reads ~44% slower at 100 K elements; `limit 1024` reads ~11% faster at 100 K but types
`656/1596` ns, so 512/64 is the frontier and the merge rule fixes `limit`/8.

Gates compare the median of at least three runs: one bytes run put clean p99 at 1262 ns against
a 1159 ns ceiling, and one string run at 1508 ns against a 1323 ns ceiling, while p50 stayed
inside its band on both shapes.

Trends that matter for future decisions: typing latency grows ~linearly with `limit` (256 → 1054 ns
p50, 1024 → 1642, 16384 → 14494); `randref` improves with `limit` (221 → 124 ns); string `scan`
favors smaller `limit`, bytes `scan` favors larger; `merge-limit` below `limit` roughly halves
typing latency and lowers the clean tail; `flat`/`load` are insensitive to both knobs. All three
shipped dictionaries now declare `merge-limit` = `limit`/8 (`notes/m0.org` §3.2 row 8).
A bytes rope costs ~1.04 bytes/elem vs ~4.16 for a string rope, and byte edits are cheaper —
relevant to `notes/m0.org` §3.3 Q18 (string chunks vs UTF-8 byte chunks).

## 10. Environment facts behind the conventions

Project conventions are stated in `AGENTS.md` (§Code style, §Typography, §Operational Constraints).
The environment facts that motivated them and are not stated there: nothing outside the project
directory is writable (`/` is not), so temp files go in `/tmp/opencode/`, and this model's output is
quantized enough that nested grammar blocks need explicit `[` brackets.

## 11. Running the oracle (dev branch) under the harness

- Extract each run: `git -C ../racket-rope archive dev | tar -x -C /tmp/opencode/rope-dev`.
- Resolve with CLI flags, not `PLTCOLLECTS` (env entries supply no source candidates):
  `racket -U -X /tmp/opencode/rope-olay -S /usr/share/racket/collects …`. `-U` drops the user
  links file (`~/.local/share/racket/9.3/links.rktd` maps `rope` -> the oracle checkout);
  `-X` replaces the main collects dir with the overlay; `-S` appends the builtin collects.
- Overlay root `/tmp/opencode/rope-olay/rope/` symlinks dev-tree entries; where dev has both a
  dir and a `.rkt` (e.g. `class`/`class.rkt`) both must exist, or nested `rope/class/*` fails.
- Require the generic core as `rope/rope`, not a bare `rope` copy: dev's `type.rkt` subclasses the
  struct types from the `rope/rope` module instance, so a second copy creates unrelated struct
  types and generic predicates (`rope-node?`, `rope-chunks`) reject typed nodes.
- `raco make` has no path flags, so the sweep runs uncompiled from source.
