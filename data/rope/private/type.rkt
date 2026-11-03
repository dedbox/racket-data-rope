#lang racket/base

(provide (all-defined-out))

(struct rope-type (name dict-id primitives) #:transparent)

(define type-keys '(empty length ref slice append width limit merge-limit chunk?))

(define (resolve-rope-type stx)
  (define v (syntax-local-value stx (λ () #f)))
  (if (rope-type? v) v (raise-syntax-error #f "not a rope type" stx)))
