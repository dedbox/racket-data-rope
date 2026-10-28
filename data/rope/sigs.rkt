#lang racket/base

;;; data/rope/sigs.rkt -- chunk signatures for the common element types.
;;;
;;; This module is the bridge between the erased rope core (data/rope/core.rkt) and
;;; Racket's own sequence types. Everything in the core is type-agnostic: the tree
;;; algorithms work the same whatever the elements are. The only type-specific part is
;;; the chunk-sig -- a sealed record of the primitive operations that must look inside a
;;; chunk. This file supplies one chunk-sig per common element type, plus the conversion
;;; helpers that turn a rope into contiguous Racket data and back again.
;;;
;;; Where it sits in the system (see notes.org):
;;;
;;;   core     the tree and its algorithms                              -- data/rope/core.rkt
;;;   sigs     concrete chunk algebras                                  -- this module
;;;   types    a nominal rope type = a chunk-sig + a compile-time index -- M1
;;;   ops      generic operations over a rope type                      -- M1+
;;;   classes  equality/ordering/etc.                                   -- M4
;;;
;;; So this is the layer a maintainer reads first when they want to know "what does a
;;; rope of characters actually hold?". The type layer wraps a sig like this one as a
;;; runtime constant plus a compile-time name; nothing here knows about types, names,
;;; or the macro layer.
;;;
;;; Reading notes:
;;;   * `σ` names a chunk-sig argument, `ρ` a rope, `c` a chunk. Same convention as core.rkt.
;;;   * Every binding is exported (all-defined-out): there are no private names here.
;;;   * A sig is a *runtime* value, not syntax. Call sites pass it explicitly, e.g.
;;;     (rope-ref string-sig ρ i). The type layer makes that passing implicit later.
;;;   * Signatures contain no equality, hashing or ordering operations on purpose. Content
;;;     comparison is a class operation in the design, and `equal?` on ropes falls back to
;;;     identity (eq?): these structs are opaque and carry no runtime type evidence.

(require data/rope/core
         racket/vector)

(provide (all-defined-out))

;; ---------------------------------------------------------------------------------------
;; Signatures for the three standard element types
;; ---------------------------------------------------------------------------------------
;;
;; Each signature is a bundle of Racket primitives over one chunk representation:
;;
;;   string-sig   chunks are strings  -- elements are chars
;;   bytes-sig    chunks are bytes    -- elements are fixnums (8-bit exact integers)
;;   vector-sig   chunks are vectors  -- elements are any value
;;
;; `width` is 1 everywhere: each element occupies exactly one index, so width == length.
;; `merge-limit` is left unspecified, so it defaults to `limit` (see make-chunk-sig in
;; core.rkt: boundary leaves are merged on concat when their combined length is <= limit).
;;
;; The limits are first-pass guesses chosen for two reasons:
;;   * small enough that a leaf copy stays within a few cache lines,
;;   * large enough that a 100 MB document is a shallow tree rather than 200k leaves.
;; They are tunables, not invariants -- the benchmark plan (notes.org §5–§6) decides them,
;; especially merge-limit, which trades "one bounded copy per append" against
;; "more, smaller leaves". Changing any of them changes only the constants below.

(define string-sig (make-chunk-sig #:empty "" #:length string-length #:ref string-ref
                                   #:slice substring #:append string-append
                                   #:width 1 #:limit 512))

(define bytes-sig (make-chunk-sig #:empty #"" #:length bytes-length #:ref bytes-ref
                                  #:slice subbytes #:append bytes-append
                                  #:width 1 #:limit 1024))

(define vector-sig (make-chunk-sig #:empty (vector) #:length vector-length #:ref vector-ref
                                   #:slice vector-copy #:append vector-append
                                   #:width 1 #:limit 128))

;; ---------------------------------------------------------------------------------------
;; Building a rope from a source value
;; ---------------------------------------------------------------------------------------
;;
;; Both constructors are O(n) and copy at most once: chunk->rope splits a chunk that is
;; larger than `limit` into bounded leaves, and reuses the chunk verbatim when it fits.
;; A source value smaller than the limit therefore becomes a single leaf holding the
;; original object -- no element copy at all.

;; (string->rope str) is string-sig's constructor: (chunk->rope string-sig str).
(define (string->rope str) (chunk->rope string-sig str))

;; (bytes->rope bstr) is bytes-sig's constructor: (chunk->rope bytes-sig bstr).
(define (bytes->rope bstr) (chunk->rope bytes-sig bstr))

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
;; They only apply to ropes built from the matching signature, because the buffer size and
;; the copy operation assume chunks are strings / bytes of that type. A vector-sig rope has
;; no flattened representation here; (rope-chunks ρ) does.

;; Returns the characters of `ρ` as one string. Requires string-sig chunks.
;; The buffer is sized by element count, which is correct because width == length for this
;; signature (see above).
(define (rope->string ρ)
  (define out (make-string (rope-length ρ)))
  (rope-fold-chunks (λ (i c) (string-copy! out i c) (+ i (string-length c))) 0 ρ)
  out)

;; Returns the bytes of `ρ` as one byte string. Requires bytes-sig chunks.
(define (rope->bytes ρ)
  (define out (make-bytes (rope-length ρ)))
  (rope-fold-chunks (λ (i c) (bytes-copy! out i c) (+ i (bytes-length c))) 0 ρ)
  out)
