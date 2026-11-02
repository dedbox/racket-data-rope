#lang racket/base

;;; data/rope/dicts.rkt -- chunk dictionaries for common element types.
;;;
;;; These chunk dictionaries demonstrate what a rope of characters actually holds. A
;;; `chunk-dict` is a **runtime** value, not syntax: call sites pass it explicitly, e.g.,
;;; (rope-ref string-dict ρ i).
;;;
;;; Per the same convention as core.rkt, `σ` names a chunk-dict argument, `ρ` a rope, and
;;; `c` a chunk.
;;;
;;; Dictionaries carry no equality, hashing, or ordering operations. `equal?` on ropes
;;; falls back to identity (`eq?`).

(require data/rope/core
         racket/vector)

(provide (all-defined-out))

;; ---------------------------------------------------------------------------------------
;; Dictionaries for the three standard element types -- Racket primitives over one chunk each
;; ---------------------------------------------------------------------------------------
;;
;;   string-dict   -- chunks are strings, elements are chars
;;   bytes-dict    -- chunks are bytes, elements are 8-bit exact integers
;;   vector-dict   -- chunks are vectors, elements are any value
;;
;; `width` is 1 everywhere: each element occupies exactly one index, so width == length.
;;
;; All three follow one rule, `merge-limit` = `limit`/8, measured rather than guessed. The
;; figures live in notes/m0.org §3.2 rows 8 and 12 and in racket-facts.md §9; the run that
;; produced them is benchmarks/limit-sweep.rkt, which ranks edits by gc-clean p99.
;;
;; The two knobs carry different costs, so they are chosen independently:
;;
;; - `limit` sets the copy bound an edit pays when it splits a leaf, and the depth of the
;;   tree a lookup walks. Editing gets cheaper as `limit` shrinks; access gets cheaper as
;;   it grows.
;;
;; - `merge-limit` decides when two boundary leaves merge on concat. Merging less pays
;;   fewer and smaller copies per edit; merging more pays one bounded copy and keeps the
;;   tree compact. Typing and patching want a small merge-limit; reads do not care,
;;   because a chunk stays bounded by `limit` either way.
;;
;; Both are tunables, not invariants, and `merge-limit` must stay ≤ `limit`: a merged
;; boundary leaf is bounded by `merge-limit`, and a leaf is bounded by `limit`.

(define string-dict (make-chunk-dict #:empty "" #:length string-length #:ref string-ref
                                     #:slice substring #:append string-append
                                     #:width 1 #:limit 512 #:merge-limit 64))

;; Byte chunks copy cheaper than string chunks, so bytes-dict takes a larger `limit`. Its
;; merge-limit moved from `limit`/4 to `limit`/8 on the rerun that measured the smaller edit
;; tail at both matched sizes. See notes/m0.org §3.2 rows 8 and 12.
(define bytes-dict (make-chunk-dict #:empty #"" #:length bytes-length #:ref bytes-ref
                                    #:slice subbytes #:append bytes-append
                                    #:width 1 #:limit 2048 #:merge-limit 256))

;; Token ropes take `limit` 128 with `merge-limit` 16 -- the measured frontier for vector
;; chunks of tokens. See notes/m0.org §3.2 row 21.
(define vector-dict (make-chunk-dict #:empty (vector) #:length vector-length #:ref vector-ref
                                     #:slice vector-copy #:append vector-append
                                     #:width 1 #:limit 128 #:merge-limit 16))

;; ---------------------------------------------------------------------------------------
;; Building a rope from a source value
;; ---------------------------------------------------------------------------------------
;;
;; Both constructors are O(n) and copy at most once: chunk->rope splits a chunk that is
;; larger than `limit` into bounded leaves, and uses the chunk directly when it fits. A
;; source value smaller than the limit therefore becomes a single leaf holding the
;; original object -- no element copy at all.

;; (string->rope str) is string-dict's constructor: (chunk->rope string-dict str).
(define (string->rope str) (chunk->rope string-dict str))

;; (bytes->rope bstr) is bytes-dict's constructor: (chunk->rope bytes-dict bstr).
(define (bytes->rope bstr) (chunk->rope bytes-dict bstr))

;; ---------------------------------------------------------------------------------------
;; Flattening a rope back into a source value
;; ---------------------------------------------------------------------------------------
;;
;; Since ropes have no generic representation and no printed content, they must be
;; materialized before their contents can be directly compared, searched, or handed to
;; code outside the library. Both of the following flattening operations are O(n): one
;; contiguous allocation plus one chunk-at-a-time copy into it. The fold's accumulator is
;; the position just past the last byte already written, so each chunk is copied once at
;; its own offset -- there is no intermediate chunk list.
;;
;; Because buffer sizes and the copy operation are presumed to belong to the same type,
;; flattening only applies to ropes built from the same dictionary. A vector-dict rope has
;; no flattened representation here; use `(rope-chunks ρ)` instead.

;; Return the characters of `ρ` as one string.
(define (rope->string ρ)
  (define out (make-string (rope-length ρ)))
  (rope-fold-chunks (λ (i c) (string-copy! out i c) (+ i (string-length c))) 0 ρ)
  out)

;; Return the bytes of `ρ` as one byte string.
(define (rope->bytes ρ)
  (define out (make-bytes (rope-length ρ)))
  (rope-fold-chunks (λ (i c) (bytes-copy! out i c) (+ i (bytes-length c))) 0 ρ)
  out)
