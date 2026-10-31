#lang racket/base

;;; tests/core-tests.rkt -- unit tests for data/rope/core.rkt.

(module+ test
  (require rackunit
           rackunit/text-ui
           racket/random
           racket/list
           data/rope/core)

  ;; =====================================================================
  ;; Fixtures and helpers
  ;; =====================================================================

  ;; A chunk-dict over strings, one element per character (width == length).
  (define (char-dict [limit 4] [merge-limit limit])
    (make-chunk-dict #:empty "" #:length string-length #:ref string-ref
                    #:slice substring #:append string-append
                    #:width 1 #:limit limit #:merge-limit merge-limit))

  ;; A chunk-dict over lists of strings: length counts strings, width counts characters,
  ;; so width and length can diverge and exercise the additive measure independently.
  (define (list-dict [limit 4])
    (make-chunk-dict #:empty '() #:length length
                    #:ref (lambda (c i) (list-ref c i))
                    #:slice (lambda (c i j) (take (drop c i) (- j i)))
                    #:append append
                    #:width (lambda (c) (apply + (map string-length c)))
                    #:limit limit))

  (define σ4 (char-dict 4))   ; default element type: chars, limit 4, merge-limit 4
  (define σ8 (char-dict 8))
  (define σw (list-dict))     ; variable width

  (define (leaf! s) (make-leaf σ4 s))

  ;; Element sequence of a rope as a list (works for any chunk-dict).
  (define (elems σ ρ)
    (for/list ([i (in-range (rope-length ρ))]) (rope-ref σ ρ i)))

  ;; Rope content as a string of characters.
  (define (strof σ ρ)
    (list->string (elems σ ρ)))

  ;; A deterministic pseudo-random string of the given length.
  (random-seed 20261027)
  (define (rnd n) (random n))
  (define (rndstr n)
    (apply string (for/list ([_ (in-range n)]) (integer->char (+ 97 (rnd 26))))))

  ;; Group fixtures (module-level defines; shared across a group's test-cases).
  (define tall
    (chunk->rope σ4 (apply string (for/list ([i (in-range 40)])
                                    (integer->char (+ 97 (modulo i 5)))))))
  (define tall2
    (chunk->rope σ4 (apply string (for/list ([i (in-range 37)])
                                    (integer->char (+ 49 (modulo i 7)))))))
  (define ρ (chunk->rope σ4 "abcdef"))        ; shared by split/slice/splice
  (define ρ-ref (chunk->rope σ4 "abcde"))     ; used by the rope-ref group

  ;; =====================================================================
  ;; empty-rope
  ;; =====================================================================
  (define suite-empty-rope
    (test-suite "empty-rope"
      (test-case "is the shared empty leaf"
        (check-true (leaf? empty-rope))
        (check-false (node? empty-rope)))
      (test-case "predicates and measures"
        (check-true (rope? empty-rope))
        (check-true (rope-empty? empty-rope))
        (check-equal? (rope-length empty-rope) 0)
        (check-equal? (rope-width empty-rope) 0)
        (check-equal? (rope-height empty-rope) 0))
      (test-case "singleton identity"
        (check-eq? empty-rope empty-rope)
        (check-eq? (make-leaf σ4 "") empty-rope))))

  ;; =====================================================================
  ;; chunk-dict construction
  ;; =====================================================================
  (define suite-chunk-dict
    (test-suite "chunk-dict"
      (test-case "constant width is wrapped as a procedure"
        (define cs (make-chunk-dict #:empty "" #:length string-length #:ref string-ref
                                   #:slice substring #:append string-append
                                   #:width 3 #:limit 10))
        (check-true (procedure? (chunk-dict-width cs)))
        (check-equal? ((chunk-dict-width cs) "abc") 9))
      (test-case "procedure width is used directly"
        (define cs (make-chunk-dict #:empty "" #:length string-length #:ref string-ref
                                   #:slice substring #:append string-append
                                   #:width (lambda (c) (string-length c)) #:limit 10))
        (check-equal? ((chunk-dict-width cs) "abc") 3))
      (test-case "merge-limit defaults to limit"
        (define cs (make-chunk-dict #:empty "" #:length string-length #:ref string-ref
                                   #:slice substring #:append string-append #:limit 7))
        (check-equal? (chunk-dict-merge-limit cs) 7))))

  ;; =====================================================================
  ;; make-leaf / make-node / rope-* accessors
  ;; =====================================================================
  (define suite-constructors
    (test-suite "constructors and accessors"
      (test-case "make-leaf measures a chunk"
        (define L (make-leaf σ4 "abc"))
        (check-true (leaf? L))
        (check-equal? (leaf-length L) 3)
        (check-equal? (leaf-width L) 3)
        (check-equal? (leaf-chunk L) "abc"))
      (test-case "make-leaf collapses a zero-length chunk to empty-rope"
        (check-eq? (make-leaf σ4 "") empty-rope))
      (test-case "make-node recomputes cached measures and shares children"
        (define L1 (make-leaf σ4 "ab"))
        (define L2 (make-leaf σ4 "cde"))
        (define N (make-node L1 L2))
        (check-true (node? N))
        (check-equal? (node-length N) 5)
        (check-equal? (node-width N) 5)
        (check-equal? (node-height N) 1)
        (check-eq? (node-left N) L1)
        (check-eq? (node-right N) L2))
      (test-case "height is one past the tallest child"
        (define L (make-leaf σ4 "abc"))
        (define N (make-node L (make-node (make-leaf σ4 "d") (make-leaf σ4 "e"))))
        (check-equal? (rope-height N) 2))
      (test-case "rope-* accessors hide the leaf/node distinction"
        (define L (make-leaf σ4 "abc"))
        (define N (make-node L (make-leaf σ4 "de")))
        (check-equal? (rope-length L) 3)
        (check-equal? (rope-length N) 5)
        (check-equal? (rope-width N) 5)
        (check-equal? (rope-height L) 0)
        (check-equal? (rope-height N) 1)
        (define-values (c0 c1) (node-children N))
        (check-eq? c0 L)
        (check-equal? (leaf-chunk c1) "de"))))

  ;; =====================================================================
  ;; chunk->rope
  ;; =====================================================================
  (define suite-chunk->rope
    (test-suite "chunk->rope"
      (test-case "empty chunk -> empty-rope"
        (check-eq? (chunk->rope σ4 "") empty-rope))
      (test-case "chunk within the limit -> single leaf"
        (define ρ (chunk->rope σ4 "abcd"))
        (check-true (leaf? ρ))
        (check-equal? (rope-length ρ) 4))
      (test-case "chunk exactly at the limit -> single leaf"
        (check-true (leaf? (chunk->rope σ8 "abcdefgh"))))
      (test-case "chunk past the limit -> node"
        (define ρ (chunk->rope σ8 "abcdefghi")) ; 9 chars, limit 8
        (check-true (node? ρ))
        (check-equal? (rope-length ρ) 9))
      (test-case "large chunk is balanced, correct, and logarithmic in height"
        (define s (apply string (for/list ([i (in-range 1000)]) #\a)))
        (define ρ (chunk->rope σ8 s))
        (check-true (rope-valid? σ8 ρ))
        (check-equal? (strof σ8 ρ) s)
        (check-true (<= (rope-height ρ) 10)) ; ~⌈1000/8⌉=125 leaves -> height ~7
        (check-equal? (rope-leaf-count ρ) 125))
      (test-case "variable width is measured independently of length"
        (define ρ (chunk->rope σw '("ab" "cde" "fghij")))
        (check-equal? (rope-length ρ) 3)
        (check-equal? (rope-width ρ) 10)
        (check-equal? (elems σw ρ) '("ab" "cde" "fghij"))
        (check-true (rope-valid? σw ρ)))))

  ;; =====================================================================
  ;; rotations
  ;; =====================================================================
  (define suite-rotations
    (test-suite "rotations"
      (test-case "rotate-left preserves the in-order sequence"
        (define ρ (make-node (make-node (leaf! "ab") (leaf! "cd"))
                             (make-node (leaf! "ef") (leaf! "gh"))))
        (check-equal? (strof σ4 (rotate-left ρ)) "abcdefgh"))
      (test-case "rotate-right preserves the in-order sequence"
        (define ρ (make-node (make-node (leaf! "ab") (leaf! "cd"))
                             (make-node (leaf! "ef") (leaf! "gh"))))
        (check-equal? (strof σ4 (rotate-right ρ)) "abcdefgh"))
      (test-case "rotate-left raises the height of a shallow tree"
        (define ρ (make-node (make-node (leaf! "ab") (leaf! "cd"))
                             (make-node (leaf! "ef") (leaf! "gh")))) ; height 2
        (check-equal? (rope-height (rotate-left ρ)) 3))))

  ;; =====================================================================
  ;; rope-join (low-level, direct)
  ;; =====================================================================
  (define suite-rope-join
    (test-suite "rope-join"
      (test-case "balanced inputs collapse to a single node"
        (define a (leaf! "abc"))
        (define b (leaf! "de"))
        (define ρ (rope-join a b))
        (check-equal? (rope-height ρ) 1)
        (check-equal? (strof σ4 ρ) "abcde"))
      (test-case "join-right (left taller) is balanced and content-preserving"
        (define ρ (rope-join tall (leaf! "zz")))
        (check-true (rope-valid? σ4 ρ))
        (check-equal? (strof σ4 ρ) (string-append (strof σ4 tall) "zz")))
      (test-case "join-left (right taller) is balanced and content-preserving"
        (define ρ (rope-join (leaf! "zz") tall))
        (check-true (rope-valid? σ4 ρ))
        (check-equal? (strof σ4 ρ) (string-append "zz" (strof σ4 tall))))
      (test-case "join of two tall ropes preserves balance and content"
        (define ρ (rope-join tall tall2))
        (check-true (rope-valid? σ4 ρ))
        (check-equal? (strof σ4 ρ) (string-append (strof σ4 tall) (strof σ4 tall2))))))

  ;; =====================================================================
  ;; rope-concat
  ;; =====================================================================
  (define suite-rope-concat
    (test-suite "rope-concat"
      (test-case "concatenation with an empty operand returns the other operand"
        (define ρ (chunk->rope σ4 "abc"))
        (check-eq? (rope-concat σ4 empty-rope ρ) ρ)
        (check-eq? (rope-concat σ4 ρ empty-rope) ρ))
      (test-case "boundary leaves merge when combined length is within merge-limit"
        (define ρ (rope-concat σ4 (leaf! "ab") (leaf! "cd"))) ; 2+2 <= 4
        (check-equal? (rope-length ρ) 4)
        (check-equal? (rope-chunks ρ) '("abcd")))
      (test-case "boundary leaves do not merge past merge-limit"
        (define ρ (rope-concat σ4 (leaf! "abc") (leaf! "de"))) ; 3+3 > 4
        (check-equal? (rope-length ρ) 5)
        (check-equal? (length (rope-chunks ρ)) 2))
      (test-case "merge-limit below limit suppresses merges that fit under the limit"
        (define σm (char-dict 10 3))                 ; limit 10, merge-limit 3
        (define ρ (chunk->rope σm "ab"))
        (define ρ2 (chunk->rope σm "cd"))
        (check-equal? (length (rope-chunks (rope-concat σm ρ ρ2))) 2)) ; 2+2 > 3
      (test-case "repeated appends keep content correct"
        (define chars (string->list "abcdefghijklmnopqr"))
        (define ρ (for/fold ([r empty-rope]) ([c chars])
                    (rope-concat σ4 r (leaf! (string c)))))
        (check-equal? (strof σ4 ρ) "abcdefghijklmnopqr"))))

  ;; =====================================================================
  ;; rope-split
  ;; =====================================================================
  (define suite-rope-split
    (test-suite "rope-split"
      (test-case "split at 0 -> (empty, ρ)"
        (define-values (a b) (rope-split σ4 ρ 0))
        (check-eq? a empty-rope)
        (check-eq? b ρ))
      (test-case "split at length -> (ρ, empty)"
        (define-values (a b) (rope-split σ4 ρ 6))
        (check-eq? a ρ)
        (check-eq? b empty-rope))
      (test-case "split in the middle preserves lengths and content"
        (define-values (a b) (rope-split σ4 ρ 2))
        (check-equal? (rope-length a) 2)
        (check-equal? (rope-length b) 4)
        (check-equal? (strof σ4 a) "ab")
        (check-equal? (strof σ4 b) "cdef"))
      (test-case "split of a single leaf yields two leaves"
        (define-values (a b) (rope-split σ4 (leaf! "abc") 2))
        (check-true (leaf? a))
        (check-true (leaf? b))
        (check-equal? (leaf-chunk a) "ab")
        (check-equal? (leaf-chunk b) "c"))
      (test-case "prefix ++ suffix reconstructs the original"
        (define-values (a b) (rope-split σ4 ρ 4))
        (check-equal? (strof σ4 (rope-concat σ4 a b)) "abcdef"))))

  ;; =====================================================================
  ;; rope-slice
  ;; =====================================================================
  (define suite-rope-slice
    (test-suite "rope-slice"
      (test-case "full range returns the same rope (structural sharing)"
        (check-eq? (rope-slice σ4 ρ 0 6) ρ))
      (test-case "empty range returns the shared empty rope"
        (check-eq? (rope-slice σ4 ρ 2 2) empty-rope)
        (check-eq? (rope-slice σ4 ρ 0 0) empty-rope))
      (test-case "partial slice has correct length and content"
        (check-equal? (strof σ4 (rope-slice σ4 ρ 1 4)) "bcd"))
      (test-case "slice starting at zero"
        (check-equal? (strof σ4 (rope-slice σ4 ρ 0 3)) "abc"))
      (test-case "slice at the end"
        (check-equal? (strof σ4 (rope-slice σ4 ρ 3 6)) "def"))
      (test-case "slice of a single leaf"
        (check-equal? (substring "abc" 1 3)
                      (strof σ4 (rope-slice σ4 (leaf! "abc") 1 3))))))

  ;; =====================================================================
  ;; rope-splice
  ;; =====================================================================
  (define suite-rope-splice
    (test-suite "rope-splice"
      (test-case "empty insertion range leaves content unchanged"
        (check-equal? (strof σ4 (rope-splice σ4 ρ 2 2 empty-rope)) "abcdef"))
      (test-case "replace a middle range"
        (define ρ* (chunk->rope σ4 "XY"))
        (check-equal? (strof σ4 (rope-splice σ4 ρ 2 4 ρ*)) "abXYef"))
      (test-case "replace a prefix range"
        (define ρ* (chunk->rope σ4 "Z"))
        (check-equal? (strof σ4 (rope-splice σ4 ρ 0 3 ρ*)) "Zdef"))
      (test-case "replace a suffix range"
        (define ρ* (chunk->rope σ4 "Z"))
        (check-equal? (strof σ4 (rope-splice σ4 ρ 3 6 ρ*)) "abcZ"))
      (test-case "replace the whole rope"
        (define ρ* (chunk->rope σ4 "PQR"))
        (check-equal? (strof σ4 (rope-splice σ4 ρ 0 6 ρ*)) "PQR"))
      (test-case "result is balanced"
        (check-true (rope-valid? σ4 (rope-splice σ4 ρ 1 5 (chunk->rope σ4 "W")))))))

  ;; =====================================================================
  ;; rope-ref
  ;; =====================================================================
  (define suite-rope-ref
    (test-suite "rope-ref"
      (test-case "refs match the source string"
        (for ([i (in-range 5)])
          (check-equal? (string-ref "abcde" i) (rope-ref σ4 ρ-ref i))))
      (test-case "ref on a single-leaf rope"
        (check-equal? (rope-ref σ4 (leaf! "hij") 1) #\i))
      (test-case "out-of-bounds ref raises"
        (check-exn exn? (lambda () (rope-ref σ4 ρ-ref 5)))
        (check-exn exn? (lambda () (rope-ref σ4 ρ-ref -1))))))

  ;; =====================================================================
  ;; fold-chunks / chunks
  ;; =====================================================================
  (define suite-fold-chunks
    (test-suite "fold-chunks and chunks"
      (test-case "fold over empty-rope returns the initial accumulator"
        (check-equal? (rope-fold-chunks (lambda (acc c) (cons c acc)) 'init empty-rope) 'init))
      (test-case "chunks are returned left-to-right"
        (define ρ (chunk->rope σ4 "abcdefgh")) ; limit 4 -> two chunks
        (check-equal? (rope-chunks ρ) '("abcd" "efgh")))
      (test-case "fold reconstructs the original content"
        (define ρ (chunk->rope σ4 "abcdefgh"))
        (define s (for/fold ([out ""]) ([c (in-list (rope-chunks ρ))]) (string-append out c)))
        (check-equal? s "abcdefgh"))
      (test-case "empty leaves contribute nothing to the fold"
        (check-equal? (rope-fold-chunks (lambda (acc c) (add1 acc)) 0 empty-rope) 0))))

  ;; =====================================================================
  ;; leaf/node counts
  ;; =====================================================================
  (define suite-counts
    (test-suite "counts"
      (test-case "leaf and node counts of a small tree"
        (define ρ (make-node (make-node (leaf! "ab") (leaf! "cd"))
                             (make-node (leaf! "ef") (leaf! "gh"))))
        (check-equal? (rope-leaf-count ρ) 4)
        (check-equal? (rope-node-count ρ) 3))
      (test-case "counts of empty-rope"
        (check-equal? (rope-leaf-count empty-rope) 0)
        (check-equal? (rope-node-count empty-rope) 0))))

  ;; =====================================================================
  ;; rope-valid?
  ;; =====================================================================
  (define suite-rope-valid?
    (test-suite "rope-valid?"
      (test-case "accepts a well-formed tree"
        (define ρ (chunk->rope σ4 "abcdefgh"))
        (check-true (rope-valid? σ4 ρ))
        (check-true (rope-valid? σ4 empty-rope)))
      (test-case "rejects a wrong cached length"
        (define bad (node 999 1 1 (make-leaf σ4 "a") (make-leaf σ4 "b")))
        (check-false (rope-valid? σ4 bad)))
      (test-case "rejects a wrong cached height"
        (define bad (node 2 2 5 (make-leaf σ4 "ab") (make-leaf σ4 "cd")))
        (check-false (rope-valid? σ4 bad)))
      (test-case "rejects siblings whose heights differ by more than one"
        (define bad (make-node (make-leaf σ4 "a")
                               (make-node (make-node (make-leaf σ4 "b") (make-leaf σ4 "c"))
                                          (make-leaf σ4 "d"))))
        (check-false (rope-valid? σ4 bad)))
      (test-case "rejects a leaf larger than the limit"
        (check-false (rope-valid? σ4 (leaf 10 10 "aaaaaaaaaa"))))
      (test-case "rejects an empty leaf that is not the top"
        (define bad (node 1 1 1 empty-rope (make-leaf σ4 "a")))
        (check-false (rope-valid? σ4 bad)))))

  ;; =====================================================================
  ;; Randomized property tests (the join/split/slice/concat invariants)
  ;;
  ;; These are the highest-coverage tests: after every random operation the tree must
  ;; stay balanced with correct cached measures (rope-valid?) and preserve its content.
  ;; A failure here means a bug in join, split, slice or concat.
  ;; =====================================================================
  (define suite-randomized
    (test-suite "randomized invariants"
      (test-case "chunk->rope always yields a valid rope"
        (for ([_ (in-range 500)])
          (define s (rndstr (rnd 60)))
          (define ρ (chunk->rope σ4 s))
          (check-true (rope-valid? σ4 ρ) "valid after chunk->rope")
          (check-equal? (strof σ4 ρ) s "content after chunk->rope")))
      (test-case "split then concat preserves content and balance"
        (for ([_ (in-range 500)])
          (define s (rndstr (rnd 60)))
          (define ρ (chunk->rope σ4 s))
          (define i (rnd (add1 (rope-length ρ))))
          (define-values (a b) (rope-split σ4 ρ i))
          (check-equal? (rope-length a) i)
          (check-equal? (rope-length b) (- (rope-length ρ) i))
          (check-true (rope-valid? σ4 a) "prefix valid")
          (check-true (rope-valid? σ4 b) "suffix valid")
          (define c (rope-concat σ4 a b))
          (check-true (rope-valid? σ4 c) "reconcat valid")
          (check-equal? (strof σ4 c) s "content preserved")))
      (test-case "concatenation of two independent ropes stays balanced"
        (for ([_ (in-range 500)])
          (define ρ1 (chunk->rope σ4 (rndstr (rnd 60))))
          (define ρ2 (chunk->rope σ4 (rndstr (rnd 60))))
          (check-true (rope-valid? σ4 ρ1))
          (check-true (rope-valid? σ4 ρ2))
          (define r1 (rope-concat σ4 ρ1 ρ2))
          (check-true (rope-valid? σ4 r1) "concat valid")
          (check-equal? (strof σ4 r1) (string-append (strof σ4 ρ1) (strof σ4 ρ2)))
          (define r2 (rope-concat σ4 ρ2 ρ1))
          (check-true (rope-valid? σ4 r2) "concat (reversed) valid")))
      (test-case "slice preserves content and balance"
        (for ([_ (in-range 500)])
          (define s (rndstr (rnd 60)))
          (define ρ (chunk->rope σ4 s))
          (define i (rnd (add1 (rope-length ρ))))
          (define j (rnd (add1 (rope-length ρ))))
          (define lo (min i j))
          (define hi (max i j))
          (define sub (rope-slice σ4 ρ lo hi))
          (check-true (rope-valid? σ4 sub) "slice valid")
          (check-equal? (strof σ4 sub) (substring s lo hi) "slice content")))
      (test-case "splice preserves balance and expected content"
        (for ([_ (in-range 500)])
          (define s (rndstr (rnd 60)))
          (define ρ (chunk->rope σ4 s))
          (define i (rnd (add1 (rope-length ρ))))
          (define j (rnd (add1 (rope-length ρ))))
          (define lo (min i j))
          (define hi (max i j))
          (define ins (rndstr (rnd 8)))
          (define ρ* (chunk->rope σ4 ins))
          (define ρout (rope-splice σ4 ρ lo hi ρ*))
          (check-true (rope-valid? σ4 ρout) "splice valid")
          (check-equal? (strof σ4 ρout)
                        (string-append (substring s 0 lo) ins (substring s hi))
                        "splice content")))))

  ;; =====================================================================
  ;; Run everything
  ;; =====================================================================
  (run-tests
   (test-suite "core"
     suite-empty-rope
     suite-chunk-dict
     suite-constructors
     suite-chunk->rope
     suite-rotations
     suite-rope-join
     suite-rope-concat
     suite-rope-split
     suite-rope-slice
     suite-rope-splice
     suite-rope-ref
     suite-fold-chunks
     suite-counts
     suite-rope-valid?
     suite-randomized)))
