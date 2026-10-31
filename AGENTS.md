# The Rope Library

A fast, pure-Racket implementation of a generalized rope data structure. Its intended
audience are Racket tool developers.

## Key Design priorities

1. Produce ultra-optimized, clean, idiomatic Racket source code with minimum response-time
   latency.
2. Provide a clear, deliberately designed user experience with minimum friction.

The target runtime is Racket v9.3 [cs].

## File system

The top-level project directory is `racket-data-rope`, a multi-lib Racket package named
`data-rope`.

* Primary sources: /data/rope/ and /data/rope.txt
* Documentation: /data/rope/scribblings/
* Tests: /tests/
* Benchmarks: /benchmarks/

## Compilation

* Everything, including package metadata and docs: `raco setup --pkgs data-rope`
* All source files: `raco setup --no-info-domain --no-docs --pkgs data-rope`
* Single source file: `raco make data/rope/some-file.rkt`

## Testing

* Organized into a hierarchy of rackunit `test-suite` definitions with individual
  `test-case`s at its leaves.
* Unit tests for data/rope/A.rkt go into tests/A-tests.rkt. Everything in this file goes
  into a top-level `(module+ test ..)`, including `require` statements.
* Integration tests for data/rope/A.rkt and data/rope/B.rkt go into tests/A--B-tests.rkt.

* To run all tests in tests/: `raco test tests/`
* To run a specific test file: `raco test tests/X.rkt`
* Always compile primary sources and test files before running tests.

## Code Coverage

* To generate a code coverage report: `raco cover -p -b data-rope`
* Results will be in the coverage/ directory.

## Benchmarking

* Keep all micro-benchmarks used to make architectural or design decisions in benchmarks/.
* Always compile primary sources and benchmark files before running benchmarks.

## Operational Constraints

* In general, do not alter files outside the top-level project directory. Put any
  temporary files into /tmp/opencode/.
* Do not install external libraries without asking.
* Do not use git push, git rebase, git merge, or remove existing git branches.
* Write macros with `define-syntax-parse-rule` whenever possible.
* Keep explanations concise. Prioritize code snippets over long paragraphs.
* When in doubt, consult the official Racket docs before guessing. There is a local copy
  of the Racket reference manual at /usr/share/doc/racket/reference/.

## Source Contents & Formatting

* Use `λ` instead of `lambda`.
* Never use unicode arrows. Always use ASCII `->`.
* Avoid LaTeX unless a formula is impossible to represent in Unicode.
* Read racket-facts.md. It contains important time-saving information on hard lessons learned.

* **Language:** Racket 9.3 CS (Chez Scheme backend).
* Write clean, macro-safe, functional, idiomatic code.
* Always utilise modern #lang racket. Avoid legacy PLT Scheme or generic Lisp code.
* Use syntax/parse for macros.
* Use pattern matching via match sparingly.
* Ban explicit state mutation (e.g., set!) except in cases where mutation would provide
  superior performance and can be encapsulated safely.
* Dynamically scoped parameters (e.g., parameterize) are ok.

## Agent Behavior & Constraints

* You are running on a tightly quantized local model. To prevent losing track of nested
  grammar blocks, you MUST use square brackets `[]` where expected by Racket idiom.

## Git commit messages

* Subject: first line only, under 68 characters — imperative, lowercase.
* Details belong in the body: blank line, then the third line onward. Body lines
  have no length limit; wrap at a readable width (~72) only for legibility.
