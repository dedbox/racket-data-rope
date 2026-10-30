#lang racket/base

;;; benchmarks/dev-shim/data/rope/sigs.rkt -- stand-in for data/rope/sigs.
;;;
;;; limit-sweep.rkt requires it; it builds every configuration through limit-harness.rkt's
;;; make-sig, so nothing needs to be exported here. Requiring it still resolves to a
;;; real module so the sweep compiles under the oracle's PLTCOLLECTS overlay.

(provide (all-defined-out))
