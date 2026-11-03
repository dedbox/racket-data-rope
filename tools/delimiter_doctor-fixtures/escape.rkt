#lang racket

(define rx #rx"pair\?: two arguments")

(displayln (regexp-match rx "pair?: two arguments"))
