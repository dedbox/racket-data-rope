#lang racket

(define (compile-message s) (list s))
(define (check-equal? a b) (cons a b))
(define (declaring v) (cons 'd v))
(define value '(a b))

(check-equal? (compile-message
               (declaring value)
               #f))
