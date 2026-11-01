#lang racket/base

;;; data/rope/dicts.rkt -- chunk dictionaries for the common element types.
;;;
;;; This module is the bridge between the erased rope core (data/rope/core.rkt) and
;;; Racket's own sequence types. Everything in the core is type-agnostic: the tree
;;; algorithms work the same whatever the elements are. The only type-specific part is
;;; the chunk algebra, implemented as a `chunk-dict`: a sealed record of the primitive
;;; operations that must look inside a chunk. This file supplies one chunk dictionary per
;;; common element type, plus the conversion helpers that turn a rope into contiguous
;;; Racket data and back again.
;;;
;;; Where it sits in the system (see M0-notes.org):
;;;
;;;   core     the tree and its algorithms                                -- data/rope/core.rkt
;;;   dicts    chunk dictionaries implementing the algebras               -- this module
;;;   types    a nominal rope type = a chunk-dict + a compile-time index  -- M1
;;;   ops      generic operations over a rope type                        -- M1+
;;;   classes  equality/ordering/etc.                                     -- M4
;;;
;;; So this is the layer a maintainer reads first when they want to know "what does a
;;; rope of characters actually hold?". The type layer wraps a dictionary like this one as
;;; a runtime constant plus a compile-time name; nothing here knows about types, names,
;;; or the macro layer.
;;;
;;; Reading notes:
;;;   * `σ` names a chunk-dict argument, `ρ` a rope, `c` a chunk. Same convention as core.rkt.
;;;   * Every binding is exported (all-defined-out): there are no private names here.
;;;   * A dictionary is a *runtime* value, not syntax. Call sites pass it explicitly, e.g.
;;;     (rope-ref string-dict ρ i). The type layer makes that passing implicit later.
;;;   * Dictionaries contain no equality, hashing or ordering operations on purpose. Content
;;;     comparison is a class operation in the design, and `equal?` on ropes falls back to
;;;     identity (eq?): these structs are opaque and carry no runtime type evidence.

(require data/rope/core
         racket/vector)

(provide (all-defined-out))

;; ---------------------------------------------------------------------------------------
;; Dictionaries for the three standard element types
;; ---------------------------------------------------------------------------------------
;;
;; Each dictionary is a bundle of Racket primitives over one chunk representation:
;;
;;   string-dict   chunks are strings  -- elements are chars
;;   bytes-dict    chunks are bytes    -- elements are fixnums (8-bit exact integers)
;;   vector-dict   chunks are vectors  -- elements are any value
;;
;; `width` is 1 everywhere: each element occupies exactly one index, so width == length.
;;
;; The limits and merge-limits are measured, not guessed -- benchmarks/limit-sweep.rkt, which
;; runs the editor pipeline (load, typing, 4-for-4 patches, deletion, paste, lexer slices and
;; chunk scans, random and sequential access, flatten) over 1 MB, 10 MB, and 100 MB buffers and
;; reports percentiles with samples that straddled a garbage collection counted separately.
;; All three dictionaries follow one rule, `merge-limit` = `limit`/8. The chosen configuration is
;; the one whose clean tail -- the p99 over samples that did not overlap a collection -- is the
;; smallest on the edit path; the read path then costs about 11% more than the best configuration
;; at 100 K elements and about 25% more at 10 MB on the bytes shape, which the edit-dominated
;; pipeline accepts. They are tunables, not invariants, and
;; `merge-limit` must stay <= `limit`: a merged boundary leaf is bounded by `merge-limit`, and a
;; leaf is bounded by `limit`.
;;
;; The two knobs carry different costs, so they are chosen independently:
;;   * `limit` sets the copy bound an edit pays when it splits a leaf, and the depth of the tree
;;     a lookup walks. Editing gets cheaper as `limit` shrinks; access gets cheaper as it grows.
;;   * `merge-limit` sets whether two boundary leaves merge on concat. Merging less pays fewer
;;     and smaller copies per edit and keeps the tail low; merging more pays one bounded copy
;;     and keeps the tree compact. Typing and patching want a small merge-limit; reads do not
;;     care, because a chunk stays bounded by `limit` either way.
;;
;; Measured on a 10 MB buffer (nanoseconds, p50 / clean p99): string-dict typing is 536/822 at
;; 512/64 against 1251/2317 at 512/512 -- merging less is the biggest latency win in this file.
;; Its read path then costs about 10% more than the best configuration: random access 203 ns
;; against 165 ns at 1024/128. Those two string figures come from the older 20-window harness.
;; The 2026-10-31 rerun at 1024 windows measured the bytes shape six times, medians: typing
;; 486/928 at 100 K elements and 399/1008 at 10 MB for 2048/256, against 483/1332 and 396/1036
;; for 2048/512; patches, deletes and pastes land within each other's tail noise; random access
;; reads at 24/41 and 88/163. Merging up to `limit` is the worst shape on the edit tail at the
;; large size -- 6.9-8.5 µs clean p99 for patches, deletes and pastes at 10 MB. Byte chunks copy
;; cheaper than string chunks, so bytes-dict's best `limit` is larger than string-dict's.
;; Retained size is payload-dominated: a rope costs about 1.04 bytes per element as bytes and
;; about 4.16 as string characters, of which the tree adds less than 0.2 bytes per element.

(define string-dict (make-chunk-dict #:empty "" #:length string-length #:ref string-ref
                                   #:slice substring #:append string-append
                                   #:width 1 #:limit 512 #:merge-limit 64))

;; bytes-dict follows the same limit/8 rule: 256 of 2048. The first sweep chose limit/4 (512),
;; but a six-run rerun on the bytes shape under the 1024-window harness measured limit/8 with the
;; smaller edit tail at both matched sizes -- 928 ns against 1332 ns clean p99 typing at 100 K,
;; and 1008 ns against 1036 ns at 10 MB -- while reads came out equal at 100 K and 2 ns slower at
;; 10 MB. See M0-notes §3.2 rows 8 and 12.
(define bytes-dict (make-chunk-dict #:empty #"" #:length bytes-length #:ref bytes-ref
                                  #:slice subbytes #:append bytes-append
                                  #:width 1 #:limit 2048 #:merge-limit 256))

;; vector-dict uses the token frontier measured by benchmarks/limit-sweep.rkt (M0-notes §3.2
;; row 21): limit 128 with merge-limit 16, which is limit/8 -- the same rule the two pipeline
;; dictionaries follow. The default merge-limit, equal to limit, measured as the worst edit
;; tail at every token size, so the field is declared rather than filled in from limit.
(define vector-dict (make-chunk-dict #:empty (vector) #:length vector-length #:ref vector-ref
                                   #:slice vector-copy #:append vector-append
                                   #:width 1 #:limit 128 #:merge-limit 16))

;; ---------------------------------------------------------------------------------------
;; Building a rope from a source value
;; ---------------------------------------------------------------------------------------
;;
;; Both constructors are O(n) and copy at most once: chunk->rope splits a chunk that is
;; larger than `limit` into bounded leaves, and reuses the chunk verbatim when it fits.
;; A source value smaller than the limit therefore becomes a single leaf holding the
;; original object -- no element copy at all.

;; (string->rope str) is string-dict's constructor: (chunk->rope string-dict str).
(define (string->rope str) (chunk->rope string-dict str))

;; (bytes->rope bstr) is bytes-dict's constructor: (chunk->rope bytes-dict bstr).
(define (bytes->rope bstr) (chunk->rope bytes-dict bstr))

;; ---------------------------------------------------------------------------------------
;; Flattening a rope back into a source value
;; ---------------------------------------------------------------------------------------
;;
;; These are the per-type serialization path of the design: a rope has no generic
;; representation and no printed content, so materializing it is how you compare, search
;; or hand it to code outside the library. Both are O(n): one contiguous allocation plus
;; one chunk-at-a-time copy into it. The fold's accumulator is the byte position already
;; written, so each chunk is copied once at its own offset -- no intermediate chunk list.
;;
;; They only apply to ropes built from the matching dictionary, because the buffer size and
;; the copy operation assume chunks are strings / bytes of that type. A vector-dict rope has
;; no flattened representation here; (rope-chunks ρ) does.

;; Returns the characters of `ρ` as one string. Requires string-dict chunks.
;; The buffer is sized by element count, which is correct because width == length for this
;; dictionary (see above).
(define (rope->string ρ)
  (define out (make-string (rope-length ρ)))
  (rope-fold-chunks (λ (i c) (string-copy! out i c) (+ i (string-length c))) 0 ρ)
  out)

;; Returns the bytes of `ρ` as one byte string. Requires bytes-dict chunks.
(define (rope->bytes ρ)
  (define out (make-bytes (rope-length ρ)))
  (rope-fold-chunks (λ (i c) (bytes-copy! out i c) (+ i (bytes-length c))) 0 ρ)
  out)
