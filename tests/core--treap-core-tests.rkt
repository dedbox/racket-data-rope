#lang racket/base

;;; tests/core--treap-core-tests.rkt -- differential tests between the two tree families.
;;;
;;; data/rope/core.rkt (AVL) and data/rope/treap-core.rkt (join-based treaps) implement the same
;;; API over the same chunk algebra. This file drives both from the *same* random operation
;;; sequence and checks they agree on content, measures and structural invariants. That is what
;;; makes the treap core safe to swap in at a `require` site: the two files are the same
;;; algorithm with a different balance mechanism, so anything the AVL core does, the treap core
;;; must do too.
;;;
;;; Shape is compared only where it is shared: `chunk->rope` splits a chunk at the same points in
;;; both files, so its leaf boundaries agree. Joins of trees of different shapes produce
;;; different shapes, so leaf boundaries are not compared there -- content and lengths are.

(module+ test
  (require rackunit
           rackunit/text-ui
           racket/random
           racket/list
           (only-in data/rope/core
                    [make-chunk-sig avl:make-chunk-sig]
                    [chunk->rope avl:chunk->rope]
                    [empty-rope avl:empty-rope]
                    [rope-empty? avl:rope-empty?]
                    [rope-length avl:rope-length]
                    [rope-concat avl:rope-concat]
                    [rope-split avl:rope-split]
                    [rope-slice avl:rope-slice]
                    [rope-splice avl:rope-splice]
                    [rope-ref avl:rope-ref]
                    [rope-chunks avl:rope-chunks]
                    [rope-valid? avl:rope-valid?])
           (only-in data/rope/treap-core
                    [make-chunk-sig treap:make-chunk-sig]
                    [chunk->rope treap:chunk->rope]
                    [empty-rope treap:empty-rope]
                    [rope-empty? treap:rope-empty?]
                    [rope-length treap:rope-length]
                    [rope-concat treap:rope-concat]
                    [rope-split treap:rope-split]
                    [rope-slice treap:rope-slice]
                    [rope-splice treap:rope-splice]
                    [rope-ref treap:rope-ref]
                    [rope-chunks treap:rope-chunks]
                    [rope-valid? treap:rope-valid?]))

  ;; =====================================================================
  ;; Fixtures
  ;; =====================================================================

  ;; One chunk algebra per implementation: identical parameters, distinct runtime values. Both
  ;; trees use a tiny chunk limit, so the tests exercise leaf boundaries.
  (define σ4 (avl:make-chunk-sig #:empty ""
                                 #:length string-length
                                 #:ref string-ref
                                 #:slice substring
                                 #:append string-append
                                 #:width 1
                                 #:limit 4))
  (define σt (treap:make-chunk-sig #:empty ""
                                   #:length string-length
                                   #:ref string-ref
                                   #:slice substring
                                   #:append string-append
                                   #:width 1
                                   #:limit 4))

  (random-seed 20261027)

  (define (rnd n) (random n))

  (define (rndstr n)
    (apply string (for/list ([_ (in-range n)]) (integer->char (+ 97 (rnd 26))))))

  ;; Content of a rope as a string, per implementation.
  (define (strof-avl ρ)
    (list->string (for/list ([i (in-range (avl:rope-length ρ))]) (avl:rope-ref σ4 ρ i))))

  (define (strof-treap ρ)
    (list->string (for/list ([i (in-range (treap:rope-length ρ))]) (treap:rope-ref σt ρ i))))

  ;; =====================================================================
  ;; Differential tests
  ;; =====================================================================

  (define suite-differential
    (test-suite "core against treap-core"

      (test-case "chunk->rope builds identical content and identical leaf boundaries"
        (for ([_ (in-range 400)])
          (define s (rndstr (rnd 60)))
          (define a (avl:chunk->rope σ4 s))
          (define t (treap:chunk->rope σt s))
          (check-true (avl:rope-valid? σ4 a) "avl valid")
          (check-true (treap:rope-valid? σt t) "treap valid")
          (check-equal? (avl:rope-length a) (treap:rope-length t))
          (check-equal? (strof-avl a) (strof-treap t))
          (check-equal? (avl:rope-chunks a) (treap:rope-chunks t) "same leaf boundaries")))

      (test-case "concat agrees on content, length and emptiness"
        (for ([_ (in-range 400)])
          (define s1 (rndstr (rnd 40)))
          (define s2 (rndstr (rnd 40)))
          (define a1 (avl:chunk->rope σ4 s1))
          (define t1 (treap:chunk->rope σt s1))
          (define a2 (avl:chunk->rope σ4 s2))
          (define t2 (treap:chunk->rope σt s2))
          (define ac (avl:rope-concat σ4 a1 a2))
          (define tc (treap:rope-concat σt t1 t2))
          (check-true (avl:rope-valid? σ4 ac) "concat avl valid")
          (check-true (treap:rope-valid? σt tc) "concat treap valid")
          (check-equal? (avl:rope-empty? ac) (treap:rope-empty? tc))
          (check-equal? (avl:rope-length ac) (treap:rope-length tc))
          (check-equal? (strof-avl ac) (string-append s1 s2))
          (check-equal? (strof-avl ac) (strof-treap tc))))

      (test-case "split agrees on both halves"
        (for ([_ (in-range 400)])
          (define s (rndstr (rnd 40)))
          (define i (rnd (add1 (string-length s))))
          (define-values (pa pb) (avl:rope-split σ4 (avl:chunk->rope σ4 s) i))
          (define-values (ta tb) (treap:rope-split σt (treap:chunk->rope σt s) i))
          (check-equal? (avl:rope-length pa) (treap:rope-length ta))
          (check-equal? (avl:rope-length pb) (treap:rope-length tb))
          (check-equal? (strof-avl pa) (strof-treap ta))
          (check-equal? (strof-avl pb) (strof-treap tb))
          (check-equal? (strof-avl pa) (substring s 0 i))
          (check-equal? (strof-avl pb) (substring s i))))

      (test-case "slice agrees on content"
        (for ([_ (in-range 400)])
          (define s (rndstr (rnd 40)))
          (define i (rnd (add1 (string-length s))))
          (define j (rnd (add1 (string-length s))))
          (define lo (min i j))
          (define hi (max i j))
          (define a (avl:rope-slice σ4 (avl:chunk->rope σ4 s) lo hi))
          (define t (treap:rope-slice σt (treap:chunk->rope σt s) lo hi))
          (check-true (avl:rope-valid? σ4 a))
          (check-true (treap:rope-valid? σt t))
          (check-equal? (avl:rope-length a) (treap:rope-length t))
          (check-equal? (strof-avl a) (substring s lo hi))
          (check-equal? (strof-avl a) (strof-treap t))))

      (test-case "splice agrees on content, including past the merge-limit"
        (for ([_ (in-range 400)])
          (define s (rndstr (rnd 40)))
          (define i (rnd (add1 (string-length s))))
          (define j (rnd (add1 (string-length s))))
          (define ins (rndstr (rnd 10)))
          (define lo (min i j))
          (define hi (max i j))
          (define a (avl:rope-splice σ4 (avl:chunk->rope σ4 s) lo hi (avl:chunk->rope σ4 ins)))
          (define t (treap:rope-splice σt (treap:chunk->rope σt s) lo hi (treap:chunk->rope σt ins)))
          (define expected (string-append (substring s 0 lo) ins (substring s hi)))
          (check-true (avl:rope-valid? σ4 a) "splice avl valid")
          (check-true (treap:rope-valid? σt t) "splice treap valid")
          (check-equal? (avl:rope-length a) (treap:rope-length t))
          (check-equal? (strof-avl a) expected)
          (check-equal? (strof-treap t) expected)))

      (test-case "a shared edit sequence keeps both trees valid and identical"
        (random-seed 20261028)
        (define model (rndstr 400))
        (define out
          (let loop ([a (avl:chunk->rope σ4 model)]
                     [t (treap:chunk->rope σt model)]
                     [m model]
                     [k 0])
            (if (>= k 300)
                (list a t m)
                (let ([i (rnd (- (string-length m) 8))]
                      [ins (rndstr 8)])
                  (loop (avl:rope-splice σ4 a i (+ i 8) (avl:chunk->rope σ4 ins))
                        (treap:rope-splice σt t i (+ i 8) (treap:chunk->rope σt ins))
                        (string-append (substring m 0 i) ins (substring m (+ i 8)))
                        (add1 k))))))
        (define a (first out))
        (define t (second out))
        (define m (third out))
        (check-true (avl:rope-valid? σ4 a) "avl valid after the edit run")
        (check-true (treap:rope-valid? σt t) "treap valid after the edit run")
        (check-equal? (strof-avl a) m "avl content matches the model")
        (check-equal? (strof-treap t) m "treap content matches the model")
        (check-equal? (avl:rope-length a) (treap:rope-length t)))))

  ;; =====================================================================
  ;; Run everything
  ;; =====================================================================
  (run-tests suite-differential))
