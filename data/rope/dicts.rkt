#lang racket/base

;;; data/rope/dicts.rkt -- chunk dictionaries for common element types.
;;;
;;; These chunk dictionaries demonstrate what a rope of characters actually holds. The
;;; actual `chunk-dict` value is a **runtime** value, not syntax -- call sites pass it
;;; explicitly, e.g., (rope-ref string-dict ρ i), and the type layer makes that passing
;;; implicit.
;;;
;;; Per the same convention as core.rkt, `σ` names a chunk-dict argument, `ρ` a rope, and
;;; `c` a chunk.
;;;
;;; Dictionaries do not contain equality, hashing, or ordering operations. Content
;;; comparison is a class operation, and `equal?` on ropes falls back to identity (eq?).

(require data/rope/core
         racket/vector)

(provide (all-defined-out))

;; ---------------------------------------------------------------------------------------
;; Dictionaries for the three standard element types
;; ---------------------------------------------------------------------------------------
;;
;; Each dictionary is a bundle of Racket primitives over one chunk representation:
;;
;;   string-dict   -- chunks are strings, elements are chars
;;   bytes-dict    -- chunks are bytes, elements are 8-bit exact integers
;;   vector-dict   -- chunks are vectors, elements are any value
;;
;; `width` is 1 everywhere: each element occupies exactly one index, so width == length.
;;
;; The limits and merge-limits are measured, not guessed -- see benchmarks/limit-sweep.rkt,
;; which runs the editor pipeline (load, typing, 4-for-4 patches, deletion, paste, lexer
;; slices and chunk scans, random and sequential access, flatten) over 1 MB, 10 MB, and 100
;; MB buffers and reports percentiles with samples that straddled a garbage collection
;; counted separately. All three dictionaries follow one rule, `merge-limit` = `limit`/8.
;; The chosen configuration is the one whose clean tail -- the p99 over samples that did not
;; overlap a collection -- is the smallest on the edit path; the read path then costs about
;; 11% more than the best configuration at 100 K elements and about 25% more at 10 MB on
;; the bytes shape, which the edit-dominated pipeline accepts. They are tunables, not
;; invariants, and `merge-limit` must stay <= `limit`: a merged boundary leaf is bounded by
;; `merge-limit`, and a leaf is bounded by `limit`.
;;
;; The two knobs carry different costs, so they are chosen independently:
;;
;; - `limit` sets the copy bound an edit pays when it splits a leaf, and the depth of the
;;   tree a lookup walks. Editing gets cheaper as `limit` shrinks; access gets cheaper as
;;   it grows.
;;
;; - `merge-limit` determines when two boundary leaves will merge on concat. Merging less
;;   pays fewer and smaller copies per edit and keeps the tail low; merging more pays one
;;   bounded copy and keeps the tree compact. Typing and patching want a small merge limit;
;;   reads do not care, because a chunk stays bounded by `limit` either way.
;;
;; Measured on a 10 MB buffer (nanoseconds, p50 / clean p99), we see string-dict typing
;; latencies of 536/822 at 512/64 (`limit`/`merge-limit`) versus 1251/2317 at 512/512. The
;; takeaway is that merging less often is a major latency win for files at this size.
;; Reading then costs about 10% more than the best configuration: random access 203 ns
;; against 165 ns at 1024/128 limits. These string figures come from and older 20-window
;; harness.
;;
;; A rerun at 1024 windows measuring `bytes-dict` six times showed that median latency for
;; typing with 2048/256 limits were 486/928 at 100 K elements and 399/1008 at 10 MB,
;; against 483/1332 and 396/1036 for 2048/512 limits. Patches, deletes, and pastes were
;; within each other's noise in the tail. Random access read latencies came in at 24/41
;; and 88/163. The same six-run reruns on strings showed typing latencies at 512/64 limits
;; of 544/1080 at 100 K and 515.5/1150.5 at 10 MB, with respective read latencies of
;; 29/80.5 and 139/219.
;;
;; Merging up to `limit` was the worst shape on the edit tail at large sizes -- 6.9-8.5 µs
;; for gc-clean p99 on patches, deletes, and pastes at 10 MB.
;;
;; Copying byte chunks is cheaper than string chunks, so bytes-dict's best `limit` is
;; larger than string-dict's. On strings, `limit 256` makes typing latency ~11% cheaper
;; than 512, but reads become ~44% slower at 100 K elements, and `limit 1024` makes reads
;; ~11% faster there while typing costs 656/1596 ns. 512/64 limits are the sweet spot for
;; balanced read/write latencies.
;;
;; We also saw that retained tree size is dominated by payload. A rope costs about 1.04
;; bytes per element as bytes and about 4.16 as string characters, of which the tree adds
;; less than 0.2 bytes per element.

(define string-dict (make-chunk-dict #:empty "" #:length string-length #:ref string-ref
                                     #:slice substring #:append string-append
                                     #:width 1 #:limit 512 #:merge-limit 64))

;; bytes-dict follows the same limit/8 rule: 256 of 2048. The first sweep chose limit/4
;; (512), but a six-run rerun on bytes-dict under the 1024-window harness measured limit/8
;; with the smaller edit tail at both sizes -- 928 ns against 1332 ns gc-clean p99 typing
;; at 100 K, and 1008 ns against 1036 ns at 10 MB -- while reads came out equal at 100 K
;; and 2 ns slower at 10 MB. See M0-notes §3.2 rows 8 and 12.
(define bytes-dict (make-chunk-dict #:empty #"" #:length bytes-length #:ref bytes-ref
                                  #:slice subbytes #:append bytes-append
                                  #:width 1 #:limit 2048 #:merge-limit 256))

;; vector-dict uses limit 128 with merge-limit 16, roughly following the limit/8 rule
;; derived from the other two chunk dictionaries (see M0-notes §3.2 row 21).
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

;; Returns the bytes of `ρ` as one byte string.
(define (rope->bytes ρ)
  (define out (make-bytes (rope-length ρ)))
  (rope-fold-chunks (λ (i c) (bytes-copy! out i c) (+ i (bytes-length c))) 0 ρ)
  out)
