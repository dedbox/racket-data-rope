#lang racket/base

;;; benchmarks/treap-bench.rkt -- measured comparison of data/rope/core.rkt (AVL) with
;;; data/rope/treap-core.rkt (join-based treaps).
;;;
;;; Both cores expose the same API over the same chunk algebra, so one workload runs against both
;;; unchanged. This script times each operation and reports the shape each core produced.
;;;
;;;   racket benchmarks/treap-bench.rkt
;;;
;;; Latency is µs/op. The shape column lists depth, leaves and nodes of the tree the workload
;;; builds; the AVL core caches depth, the treap core recomputes it on demand (O(n, diagnostic),
;;; used only here and in tests), so shape is measured once per workload, not inside the loop.
;;;
;;; How to read it: the walk cost is identical in both cores -- the same navigation, the same
;;; measure cache -- so the delta is the repair step. AVL repairs with two height comparisons and
;;; a branch per level; the treap repairs with one priority comparison per level and no height
;;; field, paid for by a `random` draw per node it creates (see benchmarks/priority-bench.rkt).
;;;
;;; Depth: AVL's is a worst-case bound. The treap's is one sample of a probabilistic distribution
;;; whose expectation is O(log n), so `depth spread` reruns the append workload under several seeds
;;; and prints the observed range.
;;;
;;; The numbers on this machine (Racket 9.3 [cs]), recorded for the decision. Input: one 78026
;;; character chunk, limit 64, 10000 ops per case.
;;;
;;;   op                        avl        treap       treap ÷ avl
;;;   chunk->rope               96.3 µs    170.4 µs    1.77
;;;   rope-concat               0.11 µs    0.09 µs     0.79
;;;   rope-split                0.32 µs    1.30 µs     4.12
;;;   rope-slice                0.29 µs    1.30 µs     4.52
;;;   rope-splice               0.77 µs    1.86 µs     2.42
;;;   rope-ref (random index)   0.13 µs    0.13 µs     1.02
;;;   rope-fold-chunks          6118 µs    6083 µs     0.99
;;;
;;;   shape of the built tree       avl        treap
;;;   depth of chunk->rope          11         11
;;;   depth after 100 appends       12         16
;;;   leaves / nodes                1226/1225  1226/1225
;;;   depth spread, 200 appends     12         13-15 over 5 seeds
;;;
;;; Reading it: `chunk->rope` is slower because it draws a priority per node it builds (1219 draws
;;; at ~76 ns each, see benchmarks/priority-bench.rkt). `rope-split`, `rope-slice` and
;;; `rope-splice` are slower for the same reason -- every join step draws a priority, and the treap
;;; descends more levels than AVL repairs. Navigation (`rope-ref`) and a whole-rope walk are the
;;; same cost in both cores, as expected: the walk is identical, and `rope-concat` with a merged
;;; boundary chunk is slightly cheaper because the treap's join step is a single comparison.

(require racket/list
           racket/random
           (only-in data/rope/core
                    [make-chunk-sig avl:make-chunk-sig]
                    [chunk->rope avl:chunk->rope]
                    [rope-concat avl:rope-concat]
                    [rope-split avl:rope-split]
                    [rope-slice avl:rope-slice]
                    [rope-splice avl:rope-splice]
                    [rope-ref avl:rope-ref]
                    [rope-height avl:rope-height]
                    [rope-length avl:rope-length]
                    [rope-leaf-count avl:rope-leaf-count]
                    [rope-node-count avl:rope-node-count]
                    [rope-fold-chunks avl:rope-fold-chunks])
           (only-in data/rope/treap-core
                    [make-chunk-sig treap:make-chunk-sig]
                    [chunk->rope treap:chunk->rope]
                    [rope-concat treap:rope-concat]
                    [rope-split treap:rope-split]
                    [rope-slice treap:rope-slice]
                    [rope-splice treap:rope-splice]
                    [rope-ref treap:rope-ref]
                    [rope-height treap:rope-height]
                    [rope-length treap:rope-length]
                    [rope-leaf-count treap:rope-leaf-count]
                    [rope-node-count treap:rope-node-count]
                    [rope-fold-chunks treap:rope-fold-chunks]))

;; One chunk algebra, identical parameters for both cores: one index per character, leaves of at
;; most 64 characters, boundary chunks merged while their combined length is ≤ 64.
(define σ4 (avl:make-chunk-sig #:empty ""
                               #:length string-length
                               #:ref string-ref
                               #:slice substring
                               #:append string-append
                               #:width 1
                               #:limit 64))
(define σt (treap:make-chunk-sig #:empty ""
                                 #:length string-length
                                 #:ref string-ref
                                 #:slice substring
                                 #:append string-append
                                 #:width 1
                                 #:limit 64))

;; µs per operation, over `n` calls of `thunk`.
(define (µs/op n thunk)
  (define t0 (current-inexact-milliseconds))
  (for ([_ (in-range n)]) (thunk))
  (/ (* 1000 (- (current-inexact-milliseconds) t0)) n))

;; Two decimal places, printed as a short decimal string.
(define (fmt x) (format "~a" (/ (round (* x 100)) 100)))

;; Times the same workload on both cores and prints the two latencies and their ratio.
(define (bench label thunk-avl thunk-treap)
  (define a (µs/op 10000 thunk-avl))
  (define t (µs/op 10000 thunk-treap))
  (printf "~a\n  avl:   ~a µs/op\n  treap: ~a µs/op\n  treap ÷ avl: ~a\n"
          label (fmt a) (fmt t) (fmt (/ t a)))
  (void))

;; Prints the shape of both trees.
(define (shape avl-ρ treap-ρ)
  (printf "  avl:   depth ~a, leaves ~a, nodes ~a\n  treap: depth ~a, leaves ~a, nodes ~a\n"
          (avl:rope-height avl-ρ)
          (avl:rope-leaf-count avl-ρ)
          (avl:rope-node-count avl-ρ)
          (treap:rope-height treap-ρ)
          (treap:rope-leaf-count treap-ρ)
          (treap:rope-node-count treap-ρ))
  (void))

;; ---------------------------------------------------------------------------------------
;; Workloads
;; ---------------------------------------------------------------------------------------

;; A long single chunk: building a rope from it is the tree every walk below navigates.
(define big
  (let loop ([s ""] [k 0])
    (cond [(= k 3000) (string-append s "abcdefghijklmnopqrstuvwxyz")]
          [else (loop (string-append s "abcdefghijklmnopqrstuvwxyz") (add1 k))])))
(define N (string-length big))

;; Appends `k` four-character chunks. This is the hot path of editing: each append merges into
;; the last leaf or joins a new tree, and the shape of the result is what the core decides.
(define (avl-append ρ0 k)
  (let loop ([ρ ρ0] [k k])
    (cond [(= k 0) ρ]
          [else (loop (avl:rope-concat σ4 ρ (avl:chunk->rope σ4 "abcd")) (sub1 k))])))
(define (treap-append ρ0 k)
  (let loop ([ρ ρ0] [k k])
    (cond [(= k 0) ρ]
          [else (loop (treap:rope-concat σt ρ (treap:chunk->rope σt "abcd")) (sub1 k))])))

;; ---------------------------------------------------------------------------------------
;; Sanity: the cores must agree, otherwise the timings compare nothing
;; ---------------------------------------------------------------------------------------

(random-seed 20261027)
(define ρa (avl:chunk->rope σ4 big))
(define ρt (treap:chunk->rope σt big))
(define append-a (avl-append ρa 100))
(define append-t (treap-append ρt 100))

(define (strof-avl ρ)
  (list->string (for/list ([i (in-range (avl:rope-length ρ))]) (avl:rope-ref σ4 ρ i))))
(define (strof-treap ρ)
  (list->string (for/list ([i (in-range (treap:rope-length ρ))]) (treap:rope-ref σt ρ i))))

(define (agree label a t)
  (if (equal? a t)
      (printf "  agree: ~a\n" label)
      (error 'treap-bench "cores disagree on ~a" label)))

(agree "chunk->rope content" (avl:rope-fold-chunks string-append "" ρa)
       (treap:rope-fold-chunks string-append "" ρt))
(agree "append content" (strof-avl append-a) (strof-treap append-t))
(agree "append length" (avl:rope-length append-a) (treap:rope-length append-t))

;; ---------------------------------------------------------------------------------------
;; Latency
;; ---------------------------------------------------------------------------------------

(printf "\n=== latency: 10000 ops each, limit 64, input ~a characters ===\n" N)

(bench "chunk->rope"
       (λ () (avl:chunk->rope σ4 big))
       (λ () (treap:chunk->rope σt big)))

;; Concat of a fresh chunk: the join's spine walk plus the boundary-leaf merge test.
(bench "rope-concat"
       (λ () (avl:rope-concat σ4 ρa (avl:chunk->rope σ4 "abcdefghij")))
       (λ () (treap:rope-concat σt ρt (treap:chunk->rope σt "abcdefghij"))))

;; Split at a midpoint: a path copy plus two joins.
(bench "rope-split"
       (λ () (avl:rope-split σ4 ρa (quotient N 2)))
       (λ () (treap:rope-split σt ρt (quotient N 2))))

(bench "rope-slice"
       (λ () (avl:rope-slice σ4 ρa (quotient N 4) (quotient (* 3 N) 4)))
       (λ () (treap:rope-slice σt ρt (quotient N 4) (quotient (* 3 N) 4))))

;; Splice replaces a small range: two splits, a join, a merge.
(bench "rope-splice"
       (λ () (avl:rope-splice σ4 ρa (quotient N 2) (+ (quotient N 2) 10) (avl:chunk->rope σ4 "XY")))
       (λ () (treap:rope-splice σt ρt (quotient N 2) (+ (quotient N 2) 10) (treap:chunk->rope σt "XY"))))

;; Random access: one navigation per index, then one chunk read.
(define (rnd n) (random n))
(random-seed 20261029)
(bench "rope-ref (random index)"
       (λ () (avl:rope-ref σ4 ρa (rnd N)))
       (λ () (treap:rope-ref σt ρt (rnd N))))

;; Whole-rope walk: fold every chunk into a string.
(bench "rope-fold-chunks"
       (λ () (avl:rope-fold-chunks string-append "" ρa))
       (λ () (treap:rope-fold-chunks string-append "" ρt)))

;; ---------------------------------------------------------------------------------------
;; Shape
;; ---------------------------------------------------------------------------------------

(printf "\n=== shape: chunk->rope of the ~a-character chunk ===\n" N)
(shape ρa ρt)

(printf "\n=== shape: 100 four-character appends onto the rope above ===\n")
(shape append-a append-t)

;; The treap's depth is probabilistic, so the spread over seeds is what its expected bound
;; covers in practice; AVL's is fixed.
(printf "\n=== depth spread: 200 appends onto the chunk, under 5 seeds ===\n")
(for ([seed (in-range 1 6)])
  (random-seed seed)
  (printf "  seed ~a: avl depth ~a, treap depth ~a\n"
          seed
          (avl:rope-height (avl-append (avl:chunk->rope σ4 big) 200))
          (treap:rope-height (treap-append (treap:chunk->rope σt big) 200))))
