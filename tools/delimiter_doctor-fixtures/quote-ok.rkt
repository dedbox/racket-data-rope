#lang racket

(define (assert x msg)
  (check-exn msg (λ () (run "abcdef
    (check-exn "other" (λ () (void 1)))
    (displayln "done")))
  (void x)))

(define (run s) s)
(define (check-exn m thunk) (thunk))

(assert 1 "first")
