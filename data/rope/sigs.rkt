#lang racket/base

;;; Chunk signatures for common element types.

(require data/rope/core
         racket/vector)

(provide (all-defined-out))

(define string-sig (make-chunk-sig #:empty "" #:length string-length #:ref string-ref
                                   #:slice substring #:append string-append
                                   #:width 1 #:limit 512))

(define bytes-sig (make-chunk-sig #:empty #"" #:length bytes-length #:ref bytes-ref
                                  #:slice subbytes #:append bytes-append
                                  #:width 1 #:limit 1024))

(define vector-sig (make-chunk-sig #:empty (vector) #:length vector-length #:ref vector-ref
                                   #:slice vector-copy #:append vector-append
                                   #:width 1 #:limit 128))

(define (string->rope str) (chunk->rope string-sig str))
(define (bytes->rope bstr) (chunk->rope bytes-sig bstr))

(define (rope->string ρ)
  (define out (make-string (rope-length ρ)))
  (rope-fold-chunks (λ (i c) (string-copy! out i c) (+ i (string-length c))) 0 ρ)
  out)

(define (rope->bytes ρ)
  (define out (make-bytes (rope-length ρ)))
  (rope-fold-chunks (λ (i c) (bytes-copy! out i c) (+ i (bytes-length c))) 0 ρ)
  out)
