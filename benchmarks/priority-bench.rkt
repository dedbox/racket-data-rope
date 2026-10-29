#lang racket/base

;;; benchmarks/priority-bench.rkt -- cost of priority generation, the decision recorded in
;;; data/rope/treap-core.rkt.
;;;
;;; A treap draws one priority per node it creates. Two generators were measured: Racket's own
;;; `random`, and a hand-rolled xorshift step written below. The winner is `random`: it is
;;; idiomatic, it is reproducible under `random-seed` (which every randomized test in tests/
;;; relies on), and its draw is one `random` call per node rather than a private PRNG state.
;;;
;;;   racket benchmarks/priority-bench.rkt
;;;
;;; The numbers on this machine (Racket 9.3 [cs]), recorded for the decision:
;;;
;;;   random 2^31  ~76 ns/draw     xor-step (folded) ~16 ns/draw   fold baseline ~1 ns/draw
;;;   random 2^30  ~64 ns/draw     xor-step net      ~15 ns/draw
;;;   random 2^16  ~55 ns/draw
;;;
;;; A hand-rolled generator is ~5× cheaper, but it is a private state machine that cannot be
;;; seeded from outside, and the draw sits inside a node allocation, so it is not on the critical
;;; path either way. `random` rejects an exact range larger than its integer range, so 2^32 is not
;;; a valid bound -- 2^31 is the largest usable one (see `priority-top`).
;;;
;;; Every measurement sinks its result so the compiler cannot dead-code-eliminate the call: a
;;; plain `for` loop whose body has no effect is optimized away, which is why the threaded fold is
;;; compared against a fold baseline rather than against nothing.

(require racket/random)

;; Range of a signed 64-bit fixnum, so the state stays in the fixnum fast path.
(define MASK (sub1 (expt 2 63)))

;; A pure xorshift step: three shifts, three xor's, no mutation -- the state is threaded through
;; the fold instead of kept in a box.
(define (xor-step [s 0])
  (define v0 (bitwise-and MASK s))
  (define v1 (bitwise-xor v0 (arithmetic-shift v0 -7)))
  (define v2 (bitwise-xor v1 (arithmetic-shift v1 -14)))
  (bitwise-and MASK (bitwise-xor v2 (arithmetic-shift v2 -7))))

;; ns/draw of a side-effecting generator (`random` touches the PRNG state, so a bare loop of it is
;; not eliminated).
(define (ns/op n thunk)
  (define t0 (current-inexact-milliseconds))
  (for ([_ (in-range n)]) (thunk))
  (/ (* 1e6 (- (current-inexact-milliseconds) t0)) n))

;; ns/draw of a pure step threaded through `for/fold`. The final state is compared below, which
;; keeps the step alive.
(define (ns/op-fold n step init)
  (define t0 (current-inexact-milliseconds))
  (define s (for/fold ([s init]) ([_ (in-range n)]) (step s)))
  (define ms (- (current-inexact-milliseconds) t0))
  (when (> s init) (void))
  (/ (* 1e6 ms) n))

(define (report label thunk)
  (printf "~a: ~a ns/draw\n" label (inexact->exact (round (ns/op 1000000 thunk)))))

(define (report-fold label step)
  (printf "~a: ~a ns/draw\n" label (inexact->exact (round (ns/op-fold 1000000 step 88672833040741751)))))

;; Warm up so the first measurement is not JIT-compiled work.
(for ([_ (in-range 200000)]) (random 65536))

(report      "random 2^31        " (λ () (random (expt 2 31))))
(report      "random 2^30        " (λ () (random (expt 2 30))))
(report      "random 2^16        " (λ () (random 65536)))
(report      "random 4           " (λ () (random 4)))
(report-fold "xor-step (folded)  " xor-step)
(report-fold "fold baseline      " (λ (s) s))

;; The hand-rolled generator measured through a fold, less the fold's own overhead.
(define raw (ns/op-fold 1000000 xor-step 88672833040741751))
(define baseline (ns/op-fold 1000000 (λ (s) s) 88672833040741751))
(printf "xor-step net         : ~a ns/draw\n" (inexact->exact (round (- raw baseline))))
