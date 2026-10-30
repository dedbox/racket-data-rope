#lang racket/base

;;; benchmarks/dev-shim/data/rope/core.rkt -- the oracle library under the harness's names.
;;;
;;; This module stands in for data/rope/core when benchmarks/dev-sweep.sh runs
;;; benchmarks/limit-sweep.rkt against the original library (../racket-rope, dev branch).
;;;
;;; The two collections have disjoint prefixes: the oracle roots at `rope`, the
;;; reimplementation at `data/rope`. So one PLTCOLLECTS entry pointing at an oracle checkout
;;; resolves `rope/*`, and one pointing at benchmarks/dev-shim resolves `data/rope/*`. Nothing
;;; collides, and the two libraries coexist in one process -- the shim below is the bridge that
;;; exposes the names the harness calls over the oracle's per-type operations.
;;;
;;; The oracle's configuration is its own fixed one: chunk limit 512 for string and bytes
;;; ropes, and Boehm's depth-trigger rebalancing (rope-append2) on every editing path. Its
;;; #:limit and #:merge-limit have therefore no oracle-side counterpart -- the sweep's
;;; config arguments shape the comparison, not the oracle. The mapping is otherwise one-to-one:
;;;
;;;   empty-rope        empty-string-rope / empty-bytes-rope, chosen from the signature
;;;   chunk->rope       source->rope (balanced build over 512-element chunks)
;;;   rope-concat       rope-append2 (concat plus on-demand rebalance)
;;;   rope-slice/splice start + end -> start + count   (the oracle takes a count)
;;;   rope-ref          rope-ref          rope-length   rope-length (struct accessor)
;;;   rope-fold-chunks  fold over rope-chunks           rope-height   rope-depth
;;;   rope->string      rope->string      rope->bytes   rope->source
;;;   rope-valid?       rope-mostly-balanced?
;;;
;;; The harness threads σ through every call and hands bare `empty-rope` as a value, so the
;;; oracle-side type choice happens per call -- a signature check, a few ns against µs-sized
;;; operations.

;; dev's own type.rkt requires `rope/rope`; the typed structs subclass the struct types
;; defined by THAT module instance, so generic predicates/operations must be imported from
;; `rope/rope` -- a bare `rope` copy of rope.rkt would create unrelated struct types.
(require rope/rope                ; the oracle's structural core and balance predicate
         rope/type/string
         rope/type/bytes)

(provide (all-defined-out)
         (all-from-out rope/rope))

;; ---------------------------------------------------------------------------------------
;; A chunk signature the harness can build; the oracle's fixed configuration ignores the
;; limits, and its accessors (chunk-sig-empty, chunk-sig-length) stay honest.
(struct chunk-sig (empty length ref slice append width limit merge-limit)
  #:authentic #:sealed)

(define (make-chunk-sig #:empty e #:length len #:ref ref #:slice slice #:append app
                        #:width [width 1] #:limit limit #:merge-limit [merge-limit limit])
  (chunk-sig e len ref slice app
             (if (procedure? width) width (λ (c) (* width (len c))))
             limit merge-limit))

;; The element type a signature works over, which selects the oracle's rope type.
(define (sig->elem σ) (if (string? (chunk-sig-empty σ)) 'string 'bytes))

;; A sentinel for the harness's bare `empty-rope`, resolved per signature to the matching
;; oracle rope. The harness binds the parameter object itself, so identity checks cover both
;; the object and its stored value.
(struct dev-empty () #:transparent)
(define empty-rope (make-parameter (dev-empty)))

(define (resolve σ x)
  (define v (if (eq? x empty-rope) (empty-rope) x))
  (cond [(dev-empty? v) (if (eq? 'string (sig->elem σ)) empty-string-rope empty-bytes-rope)]
        [else v]))

;; ---------------------------------------------------------------------------------------
;; Operations, in the harness's order: (σ rope ...), returning ropes.
;; ---------------------------------------------------------------------------------------

(define (chunk->rope σ c)
  (if (eq? 'string (sig->elem σ)) (string->rope c) (bytes->rope c)))

(define (rope-concat σ l r)
  (if (eq? 'string (sig->elem σ))
      (string-rope-append2 (resolve σ l) (resolve σ r))
      (bytes-rope-append2 (resolve σ l) (resolve σ r))))

;; The oracle's slice and splice take a count, not an end.
(define (rope-slice σ ρ start end)
  (define k (- end start))
  (define ρ* (resolve σ ρ))
  (if (eq? 'string (sig->elem σ)) (string-rope-slice ρ* start k) (bytes-rope-slice ρ* start k)))

(define (rope-splice σ ρ start end insertion)
  (define k (- end start))
  (define ρ* (resolve σ ρ))
  (if (eq? 'string (sig->elem σ))
      (string-rope-splice ρ* start k (resolve σ insertion))
      (bytes-rope-splice ρ* start k (resolve σ insertion))))

;; Splits are not exercised by the sweep scenarios; kept for the differential suites that will
;; use them against the oracle later (§1.6).
(define (rope-split σ ρ i)
  (define ρ* (resolve σ ρ))
  (if (eq? 'string (sig->elem σ)) (string-rope-split ρ* i) (bytes-rope-split ρ* i)))

(define (rope-ref σ ρ i)
  (define ρ* (resolve σ ρ))
  (if (eq? 'string (sig->elem σ)) (string-rope-ref ρ* i) (bytes-rope-ref ρ* i)))

(define (rope-fold-chunks proc init ρ)
  ;; No signature in this harness API; a sentinel resolves to either oracle empty rope, since
  ;; its chunk list is empty either way.
  (define ρ* (cond [(eq? ρ empty-rope) (empty-rope)]
                   [(dev-empty? ρ) empty-string-rope]
                   [else ρ]))
  (for/fold ([acc init]) ([c (in-list (rope-chunks ρ*))]) (proc acc c)))

;; ---------------------------------------------------------------------------------------
;; Wrappers whose names collide with the new core but not with the oracle's own exports.
;; ---------------------------------------------------------------------------------------

(define (rope->string ρ) (string-rope->source ρ))
(define (rope->bytes ρ) (bytes-rope->source ρ))
(define (rope-height ρ) (rope-depth ρ))
(define (rope-leaf-count ρ) (length (rope-chunks ρ)))
(define (rope-valid? σ ρ) (rope-mostly-balanced? ρ))
