#lang info

(define collection 'multi)
(define version "1.0")
(define pkg-authors '("Eric Griffis <dedbox@gmail.com>"))
(define pkg-desc "A rope data structure.")
(define license '(MIT OR Apache-2.0))

(define deps
  '("base"))

(define build-deps
  '("racket-doc"
    "rackunit-lib"
    "scribble-lib"))
