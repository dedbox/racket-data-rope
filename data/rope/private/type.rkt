#lang racket/base

;;; data/rope/private/type.rkt -- the compile-time record that names a rope type.
;;;
;;; === What a rope-type is ===
;;;
;;; `chunk?` is the ninth operation a record holds, and it is compile-time only: macro
;;; expansion reads it to produce the runtime cast checks a declaration builds, while the
;;; dictionary a type realizes carries eight fields, so the ninth entry never becomes
;;; runtime data (ADR-018 fixes the dictionary's field set, ADR-001 keeps `chunk?` at the
;;; typing layer).
;;;
;;; === Key invariants ===
;;;
;;; `type-keys` lists the same nine names in the order of a record's `primitives`, and
;;; both put `chunk?` last. An index into either list names the same operation, so the
;;; eight dictionary fields keep indices 0 through 7 and never need shifting.

(provide (all-defined-out))

;;   name        -- the type's binding name, which is its nominal identity
;;   dict-id     -- syntax for the dictionary the type is realized by
;;   primitives  -- the nine chunk-algebra operations the type declares
(struct rope-type (name dict-id primitives) #:transparent)

(define type-keys '(empty length ref slice append width limit merge-limit chunk?))

(define (resolve-rope-type stx)
  (define v (syntax-local-value stx (λ () #f)))
  (if (rope-type? v) v (raise-syntax-error #f "not a rope type" stx)))
