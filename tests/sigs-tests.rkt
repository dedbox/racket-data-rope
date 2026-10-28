#lang racket/base

;;; tests/sigs-tests.rkt -- unit tests for data/rope/sigs.rkt.
;;;
;;; sigs.rkt is thin code: three chunk-sig values plus four converters. Most of its
;;; behaviour is inherited from data/rope/core.rkt, so these tests pin down the boundary
;;; between the two modules -- the chunk algebra each sig bundles, and what the core does
;;; with that algebra.
;;;
;;; What the tests assert:
;;;   * slice follows Racket's [start, end) convention (start and end, not start and count)
;;;   * the limits and merge-limits are the values the module declares
;;;   * width is a procedure, and for these sigs it equals the element count
;;;   * an empty chunk collapses to the shared empty-rope, and appending two empty chunks
;;;     is still an empty chunk
;;;   * append itself ignores limit; rope-concat bounds chunk size through merge-limit
;;;   * a chunk at or below the limit is stored verbatim; a chunk above it is split and
;;;     copied
;;;   * flattening accumulates chunk offsets across uneven chunks, ignores the width
;;;     measure, and produces a fresh buffer that shares nothing with a chunk
;;;   * the wrong-type error only surfaces when a chunk is copied, so an empty rope of any
;;;     sig flattens, and a mismatched one raises naming the copy
;;;   * out-of-range access reports the chunk primitive's own error, not a rope error
;;;   * a vector chunk is mutable, so a rope built from a vector aliases that vector

(module+ test
  (require rackunit
           rackunit/text-ui
           racket/vector
           data/rope/core
           data/rope/sigs)

  ;; =====================================================================
  ;; Helpers
  ;; =====================================================================

  ;; Sources with a distinct element at every position, so a swapped or off-by-one chunk
  ;; cannot pass unnoticed.
  (define (char-at i)
    (integer->char (+ 97 (modulo i 26))))

  (define (byte-at i)
    (+ 97 (modulo i 2)))

  ;; A vector element: a list, so it is distinguishable by identity as well as by value.
  (define (elem-at i)
    (list i 'sym 3.5))

  (define (str n)
    (apply string (build-list n char-at)))

  (define (byt n)
    (list->bytes (build-list n byte-at)))

  (define (vect n)
    (list->vector (build-list n elem-at)))

  (define sigs (list string-sig bytes-sig vector-sig))
  (define limits (list 512 1024 128))

  ;; The source chunk for n elements, per sig.
  (define (src σ n)
    (cond
      [(eq? σ string-sig) (str n)]
      [(eq? σ bytes-sig) (byt n)]
      [else (vect n)]))

  ;; A rope built from the chunk `c` through sig σ's converter, when one exists.
  (define (rope-from σ c)
    (cond
      [(eq? σ string-sig) (string->rope c)]
      [(eq? σ bytes-sig) (bytes->rope c)]
      [else (chunk->rope vector-sig c)]))

  ;; A rope of n elements.
  (define (build σ n)
    (rope-from σ (src σ n)))

  ;; A chunk's elements as a list: the model a rope is compared against.
  (define (elem-list σ c)
    (cond
      [(eq? σ string-sig) (string->list c)]
      [(eq? σ bytes-sig) (bytes->list c)]
      [else (vector->list c)]))

  ;; A rope's elements, left-to-right.
  (define (elems σ ρ)
    (for/list ([i (in-range (rope-length ρ))])
      (rope-ref σ ρ i)))

  ;; A rope's chunk sizes, left-to-right.
  (define (chunk-sizes σ ρ)
    (for/list ([c (in-list (rope-chunks ρ))]
               #:when (positive? ((chunk-sig-length σ) c)))
      ((chunk-sig-length σ) c)))

  ;; The largest chunk size in a rope.
  (define (largest-chunk σ ρ)
    (for/fold ([m 0]) ([c (in-list (rope-chunks ρ))])
      (max m ((chunk-sig-length σ) c))))

  ;; Chunk-level counterparts of the rope operations, for the expected content.
  (define (slice* σ c i j)
    ((chunk-sig-slice σ) c i j))

  (define (append* σ a b)
    ((chunk-sig-append σ) a b))

  ;; The chunk expected after replacing [i, j) of `s` by `ins`.
  (define (splice* σ s i j ins)
    (define n ((chunk-sig-length σ) s))
    (append* σ (append* σ (slice* σ s 0 i) ins) (slice* σ s j n)))

  ;; =====================================================================
  ;; The public surface of sigs.rkt
  ;; =====================================================================
  (define suite-surface
    (test-suite "public surface"
      (test-case "the three sigs are chunk-sig records, not ropes"
        (for ([σ (in-list sigs)])
          (check-true (chunk-sig? σ))
          (check-false (rope? σ))))
      (test-case "the converters are one-argument procedures"
        (check-true (procedure? string->rope))
        (check-true (procedure? bytes->rope))
        (check-true (procedure? rope->string))
        (check-true (procedure? rope->bytes))
        (check-equal? (procedure-arity string->rope) 1)
        (check-equal? (procedure-arity bytes->rope) 1)
        (check-equal? (procedure-arity rope->string) 1)
        (check-equal? (procedure-arity rope->bytes) 1))))

  ;; =====================================================================
  ;; Chunk algebra: the fields every sig must satisfy
  ;; =====================================================================
  (define suite-algebra
    (test-suite "chunk algebra"
      (test-case "limits and merge-limits are the declared values"
        (check-equal? (chunk-sig-limit string-sig) 512)
        (check-equal? (chunk-sig-merge-limit string-sig) 512)
        (check-equal? (chunk-sig-limit bytes-sig) 1024)
        (check-equal? (chunk-sig-merge-limit bytes-sig) 1024)
        (check-equal? (chunk-sig-limit vector-sig) 128)
        (check-equal? (chunk-sig-merge-limit vector-sig) 128))
      (test-case "merge-limit defaults to limit, which is how the sigs are written"
        (for ([σ (in-list sigs)])
          (check-equal? (chunk-sig-merge-limit σ) (chunk-sig-limit σ))))
      (test-case "width is a procedure that equals the element count"
        ;; All three sigs declare #:width 1, so width == length on every chunk.
        (for ([σ (in-list sigs)] [c (in-list (list (src string-sig 7)
                                                  (src bytes-sig 7)
                                                  (src vector-sig 7)))])
          (check-true (procedure? (chunk-sig-width σ)))
          (check-equal? ((chunk-sig-width σ) c) ((chunk-sig-length σ) c)))
        (for ([σ (in-list sigs)])
          (check-equal? ((chunk-sig-width σ) (chunk-sig-empty σ)) 0)))
      (test-case "the empty chunk per sig"
        (check-equal? (chunk-sig-empty string-sig) "")
        (check-equal? (chunk-sig-empty bytes-sig) #"")
        (check-equal? (chunk-sig-empty vector-sig) (vector))
        (for ([σ (in-list sigs)])
          (check-equal? ((chunk-sig-length σ) (chunk-sig-empty σ)) 0)))
      (test-case "a zero-length chunk never becomes a leaf"
        (for ([σ (in-list sigs)])
          (check-eq? (make-leaf σ (chunk-sig-empty σ)) empty-rope)))
      (test-case "ref returns the sig's element type"
        ;; bytes elements are fixnums, not chars
        (check-equal? ((chunk-sig-ref string-sig) "abc" 1) #\b)
        (check-equal? ((chunk-sig-ref bytes-sig) #"abc" 1) (char->integer #\b))
        (check-true (exact-integer? ((chunk-sig-ref bytes-sig) #"abc" 1)))
        ;; vector elements are any value, including false and lists
        (check-equal? ((chunk-sig-ref vector-sig) (vector #f 'a 3) 0) #f)
        (check-equal? ((chunk-sig-ref vector-sig) (vector #f 'a 3) 1) 'a))
      (test-case "slice takes start and end, as Racket's own functions do"
        (check-equal? ((chunk-sig-slice string-sig) "abcdef" 1 4) "bcd")
        (check-equal? ((chunk-sig-slice bytes-sig) #"abcdef" 1 4) #"bcd")
        (check-equal? ((chunk-sig-slice vector-sig) (vector 1 2 3 4) 1 4) (vector 2 3 4))
        ;; treating the second argument as a count would give a different chunk
        (check-equal? ((chunk-sig-slice string-sig) "abcdef" 1 2) "b")
        (check-equal? ((chunk-sig-slice string-sig) "abcdef" 1 3) "bc"))
      (test-case "slice of an empty range, and of the whole chunk"
        (for ([σ (in-list sigs)])
          (define c (src σ 5))
          (check-equal? ((chunk-sig-length σ) ((chunk-sig-slice σ) c 3 3)) 0)
          (check-equal? (elem-list σ ((chunk-sig-slice σ) c 0 5)) (elem-list σ c))))
      (test-case "slice always copies, so the result is never the original chunk"
        (check-false (eq? ((chunk-sig-slice string-sig) "abc" 0 3) "abc"))
        (check-false (eq? ((chunk-sig-slice bytes-sig) #"abc" 0 3) #"abc"))
        (define v (vector 1 2 3))
        (check-false (eq? ((chunk-sig-slice vector-sig) v 0 3) v)))
      (test-case "slice past the chunk raises from the primitive"
        (check-exn #rx"substring" (λ () ((chunk-sig-slice string-sig) "abc" 1 5)))
        (check-exn #rx"subbytes" (λ () ((chunk-sig-slice bytes-sig) #"abc" 1 5)))
        (check-exn #rx"vector-copy" (λ () ((chunk-sig-slice vector-sig) (vector 1 2) 1 5))))
      (test-case "ref of an empty or out-of-range chunk raises from the primitive"
        (check-exn #rx"string-ref" (λ () ((chunk-sig-ref string-sig) "" 0)))
        (check-exn #rx"bytes-ref" (λ () ((chunk-sig-ref bytes-sig) #"" 0)))
        (check-exn #rx"vector-ref" (λ () ((chunk-sig-ref vector-sig) (vector) 0)))
        (check-exn #rx"exact-nonnegative-integer"
                   (λ () ((chunk-sig-ref vector-sig) (vector 1) -1))))
      (test-case "append concatenates and ignores the limit"
        ;; append is a plain chunk operation and does not bound chunk size; rope-concat
        ;; bounds it through merge-limit, which is why the two limits are separate fields.
        (define merged ((chunk-sig-append string-sig) (str 512) (str 1)))
        (check-equal? ((chunk-sig-length string-sig) merged) 513)
        (check-false (rope-valid? string-sig (make-leaf string-sig merged)))
        (check-equal? ((chunk-sig-append bytes-sig) #"ab" #"cd") #"abcd")
        (check-equal? ((chunk-sig-append vector-sig) (vector 1) (vector 2 3))
                      (vector 1 2 3)))
      (test-case "appending two empty chunks is still an empty chunk"
        (for ([σ (in-list sigs)])
          (define e ((chunk-sig-append σ) (chunk-sig-empty σ) (chunk-sig-empty σ)))
          (check-equal? ((chunk-sig-length σ) e) 0)
          ;; make-leaf collapses it, so an empty merged chunk cannot enter a tree
          (check-eq? (make-leaf σ e) empty-rope)))))

  ;; =====================================================================
  ;; Construction
  ;; =====================================================================
  (define suite-construction
    (test-suite "construction"
      (test-case "an empty source is the shared empty-rope for every sig"
        (check-eq? (string->rope "") empty-rope)
        (check-eq? (bytes->rope #"") empty-rope)
        (check-eq? (chunk->rope vector-sig (vector)) empty-rope)
        (for ([σ (in-list sigs)])
          (check-eq? (build σ 0) empty-rope)
          (check-equal? (rope-length (build σ 0)) 0)
          (check-equal? (chunk-sizes σ (build σ 0)) null)))
      (test-case "a source exactly at the limit is one leaf holding the source verbatim"
        (for ([σ (in-list sigs)] [n (in-list limits)])
          (define s (src σ n))
          (define ρ (rope-from σ s))
          (check-true (leaf? ρ))
          (check-equal? (rope-length ρ) n)
          (check-equal? (rope-width ρ) n)
          (check-equal? (rope-height ρ) 0)
          (check-true (rope-valid? σ ρ))
          ;; chunk->rope reuses a chunk that spans the whole input instead of slicing it
          (check-eq? (leaf-chunk ρ) s)))
      (test-case "a source below the limit is stored verbatim (strings and bytes)"
        (define s (str 300))
        (check-eq? (leaf-chunk (string->rope s)) s)
        (define b (byt 300))
        (check-eq? (leaf-chunk (bytes->rope b)) b))
      (test-case "limit + 1 splits into two leaves and copies the chunk"
        (for ([σ (in-list sigs)] [n (in-list '(513 1025 129))] [limit (in-list limits)])
          (define s (src σ n))
          (define ρ (build σ n))
          (check-true (node? ρ))
          (check-equal? (rope-length ρ) n)
          (check-equal? (chunk-sizes σ ρ) (list limit 1))
          (check-equal? (rope-leaf-count ρ) 2)
          (check-equal? (rope-height ρ) 1)
          ;; both halves are slices, so neither leaf aliases the source
          (for ([c (in-list (rope-chunks ρ))])
            (check-false (eq? c s)))
          (check-true (rope-valid? σ ρ))))
      (test-case "chunk sizes stay within the limit for many source sizes"
        (for ([σ (in-list sigs)] [limit (in-list limits)])
          (for ([n (in-list (list 0 1 7 limit (add1 limit) 1000 2000 3073 4096))])
            (define ρ (build σ n))
            (check-true (rope-valid? σ ρ) "valid tree")
            (check-true (<= (largest-chunk σ ρ) limit) "chunk within limit")
            (check-equal? (elems σ ρ) (elem-list σ (src σ n)) "content"))))
      (test-case "leaf counts and heights match the chunk policy"
        ;; chunk->rope splits at multiples of limit, so these shapes are exact.
        (define s2000 (string->rope (str 2000)))
        (check-equal? (chunk-sizes string-sig s2000) (list 512 512 512 464))
        (check-equal? (rope-leaf-count s2000) 4)
        (check-equal? (rope-height s2000) 2)
        (define b3000 (bytes->rope (byt 3000)))
        (check-equal? (chunk-sizes bytes-sig b3000) (list 1024 1024 952))
        (check-equal? (rope-leaf-count b3000) 3)
        (check-equal? (rope-height b3000) 2)
        (define v300 (chunk->rope vector-sig (vect 300)))
        (check-equal? (chunk-sizes vector-sig v300) (list 128 128 44))
        (check-equal? (rope-leaf-count v300) 3)
        (check-equal? (rope-height v300) 2))))

  ;; =====================================================================
  ;; Flattening: rope -> string and rope -> bytes
  ;; =====================================================================
  (define suite-flatten
    (test-suite "flattening"
      (test-case "an empty rope flattens to an empty source value"
        (check-equal? (rope->string empty-rope) "")
        (check-equal? (rope->bytes empty-rope) #"")
        ;; make-string/make-bytes allocate fresh, so these are not the interned literals
        (check-false (eq? (rope->string empty-rope) ""))
        (check-false (eq? (rope->bytes empty-rope) #"")))
      (test-case "round trip for one leaf and for a multi-leaf rope"
        (for ([n (in-list (list 0 1 3 300 512 513 2000 3073))])
          (define s (str n))
          (check-equal? (rope->string (string->rope s)) s)
          (define b (byt n))
          (check-equal? (rope->bytes (bytes->rope b)) b)))
      (test-case "the flattened buffer is fresh and shares nothing with a chunk"
        (define ρ (string->rope (str 2000)))
        (define out (rope->string ρ))
        (check-false (eq? out (str 2000)))
        (for ([c (in-list (rope-chunks ρ))])
          (check-false (eq? out c)))
        (define ρb (bytes->rope (byt 2000)))
        (check-false (eq? (rope->bytes ρb) (byt 2000))))
      (test-case "chunk offsets accumulate correctly across uneven chunks"
        ;; The fold's accumulator is the position already written, so uneven chunk sizes
        ;; must still place each chunk at the right offset.
        (define small (string->rope (str 300)))
        (define big (string->rope (str 512)))
        (define ρ (rope-concat string-sig (rope-concat string-sig small small) big))
        (check-equal? (chunk-sizes string-sig ρ) (list 300 300 512))
        ;; the content is the concatenation of the chunks, not a fresh source of the same
        ;; total size
        (check-equal? (rope->string ρ)
                      (append* string-sig (append* string-sig (str 300) (str 300)) (str 512)))
        (define ρb (rope-concat bytes-sig (bytes->rope (byt 500)) (bytes->rope (byt 900))))
        (check-equal? (chunk-sizes bytes-sig ρb) (list 500 900))
        (check-equal? (rope->bytes ρb) (append* bytes-sig (byt 500) (byt 900))))
      (test-case "flattening works for ropes whose chunks come from slice and splice"
        (define ρ (string->rope (str 2000)))
        (check-equal? (rope->string (rope-slice string-sig ρ 100 1400))
                      (slice* string-sig (str 2000) 100 1400))
        (define ins-chunk (str 50))
        (define ins (string->rope ins-chunk))
        (define ρ* (rope-splice string-sig ρ 300 900 ins))
        (check-equal? (rope->string ρ*)
                      (splice* string-sig (str 2000) 300 900 ins-chunk))
        (define ρs (rope-slice string-sig ρ 600 1900))
        (check-equal? (elems string-sig ρs)
                      (elem-list string-sig (slice* string-sig (str 2000) 600 1900))))
      (test-case "flattening ignores the width measure"
        ;; rope->string sizes its buffer by element count, so a sig whose elements span
        ;; several indices still flattens correctly.
        (define wide-sig (make-chunk-sig #:empty "" #:length string-length #:ref string-ref
                                         #:slice substring #:append string-append
                                         #:width 2 #:limit 4))
        (define ρ (chunk->rope wide-sig "abcdef"))
        (check-equal? (rope-length ρ) 6)
        (check-equal? (rope-width ρ) 12)
        (check-equal? (rope->string ρ) "abcdef"))
      (test-case "flattening a rope built from a mismatched sig raises"
        ;; rope->string needs chunks that are strings; rope->bytes needs bytes chunks.
        ;; The type mismatch only surfaces when a chunk is copied, so an empty rope of any
        ;; sig flattens without touching the primitives.
        (check-equal? (rope->string (chunk->rope vector-sig (vector))) "")
        (check-exn #rx"string-copy!" (λ () (rope->string (chunk->rope vector-sig (vect 5)))))
        (check-exn #rx"string-copy!" (λ () (rope->string (bytes->rope (byt 10)))))
        (check-exn #rx"bytes-copy!" (λ () (rope->bytes (string->rope (str 10))))))
      (test-case "flattening a rope whose boundary chunks merged into one leaf"
        (define ρ (rope-concat string-sig (string->rope (str 200)) (string->rope (str 200))))
        (check-equal? (chunk-sizes string-sig ρ) (list 400))
        (check-equal? (rope->string ρ) (append* string-sig (str 200) (str 200))))))

  ;; =====================================================================
  ;; Rope operations driven by the sigs
  ;; =====================================================================
  (define suite-operations
    (test-suite "operations through the sigs"
      (test-case "concat merges boundary chunks when they fit merge-limit"
        (for ([σ (in-list sigs)] [limit (in-list limits)])
          (define half (src σ (/ limit 2)))
          (define ρ (rope-concat σ (build σ (/ limit 2)) (build σ (/ limit 2))))
          (check-equal? (chunk-sizes σ ρ) (list limit))
          (check-true (rope-valid? σ ρ))
          ;; the merged chunk is the two halves concatenated, so content matches at chunk
          ;; level, not against a fresh source
          (check-equal? (elems σ ρ) (elem-list σ (append* σ half half)))))
      (test-case "concat does not merge past merge-limit"
        (for ([σ (in-list sigs)] [limit (in-list limits)])
          ;; Both boundary leaves are exactly at the limit, so their total is over limit.
          (define s (src σ limit))
          (define ρ (rope-concat σ (build σ limit) (build σ limit)))
          (check-equal? (chunk-sizes σ ρ) (list limit limit))
          (check-true (rope-valid? σ ρ))
          (check-equal? (elems σ ρ) (elem-list σ (append* σ s s)))))
      (test-case "concat with an empty operand returns the other operand unchanged"
        (for ([σ (in-list sigs)])
          (define ρ (build σ 600))
          (check-eq? (rope-concat σ empty-rope ρ) ρ)
          (check-eq? (rope-concat σ ρ empty-rope) ρ)))
      (test-case "split at every index keeps both halves valid"
        (for ([σ (in-list sigs)])
          (define n 600)
          (define s (src σ n))
          (define ρ (build σ n))
          (for ([i (in-range (add1 n))])
            (define-values (a b) (rope-split σ ρ i))
            (check-true (rope-valid? σ a) "prefix valid")
            (check-true (rope-valid? σ b) "suffix valid")
            (check-equal? (elems σ a) (elem-list σ (slice* σ s 0 i)))
            (check-equal? (elems σ b) (elem-list σ (slice* σ s i n)))
            (check-equal? (elems σ (rope-concat σ a b)) (elem-list σ s)))))
      (test-case "slice across chunk boundaries matches the chunk-level slice"
        (for ([σ (in-list sigs)] [limit (in-list limits)])
          (define n 2000)
          (define s (src σ n))
          (define ρ (build σ n))
          (for ([range (in-list (list (cons 0 0) (cons 0 n) (cons 100 1400)
                                      (cons 1500 2000) (cons 700 701) (cons 999 1001)))])
            (define i (car range))
            (define j (cdr range))
            (define sub (rope-slice σ ρ i j))
            (check-true (rope-valid? σ sub))
            (check-equal? (rope-length sub) (- j i))
            (check-equal? (elems σ sub) (elem-list σ (slice* σ s i j)))
            (check-true (<= (largest-chunk σ sub) limit)))))
      (test-case "slice of the whole range shares the rope; an empty range shares empty-rope"
        (define ρ (string->rope (str 2000)))
        (check-eq? (rope-slice string-sig ρ 0 2000) ρ)
        (check-eq? (rope-slice string-sig ρ 700 700) empty-rope))
      (test-case "splice replaces a range, staying balanced"
        (for ([σ (in-list sigs)] [limit (in-list limits)])
          (define n 2000)
          (define s (src σ n))
          (define ρ (build σ n))
          (for ([ins-n (in-list (list 0 1 7 128 400))])
            (define ins-chunk (src σ ins-n))
            (define ins (build σ ins-n))
            (define ρ* (rope-splice σ ρ 300 900 ins))
            (define expected (splice* σ s 300 900 ins-chunk))
            (check-true (rope-valid? σ ρ*) "splice valid")
            (check-equal? (rope-length ρ*) (- (+ n ins-n) 600))
            (check-equal? (elems σ ρ*) (elem-list σ expected))
            (check-true (<= (largest-chunk σ ρ*) limit)))))
      (test-case "splice at the ends and of the empty rope"
        (for ([σ (in-list sigs)])
          (define ρ (build σ 20))
          (define ins-chunk (src σ 4))
          (define ins (build σ 4))
          (check-equal? (elems σ (rope-splice σ ρ 0 0 ins))
                        (elem-list σ (append* σ ins-chunk (src σ 20))))
          (check-equal? (elems σ (rope-splice σ ρ 20 20 ins))
                        (elem-list σ (append* σ (src σ 20) ins-chunk)))
          (check-equal? (elems σ (rope-splice σ ρ 0 20 ins)) (elem-list σ ins-chunk))
          (check-equal? (elems σ (rope-splice σ empty-rope 0 0 ins)) (elem-list σ ins-chunk))))
      (test-case "ref matches the source at every index"
        (for ([σ (in-list sigs)])
          (define n 1500)
          (define s (src σ n))
          (define ρ (build σ n))
          (for ([i (in-range n)])
            (check-equal? (rope-ref σ ρ i) ((chunk-sig-ref σ) s i)))))
      (test-case "ref out of range reports the chunk primitive's error"
        (define sρ (string->rope (str 100)))
        (define bρ (bytes->rope (byt 100)))
        (define vρ (chunk->rope vector-sig (vect 100)))
        (check-exn #rx"string-ref: index is out of range"
                   (λ () (rope-ref string-sig sρ 100)))
        (check-exn #rx"bytes-ref: index is out of range"
                   (λ () (rope-ref bytes-sig bρ 100)))
        (check-exn #rx"vector-ref: index is out of range"
                   (λ () (rope-ref vector-sig vρ 100)))
        (check-exn #rx"exact-nonnegative-integer" (λ () (rope-ref string-sig sρ -1)))
        (check-exn #rx"exact-nonnegative-integer" (λ () (rope-ref vector-sig vρ -1)))
        ;; ref of the empty rope: its chunk is #f, so the primitive's type error surfaces
        (check-exn #rx"string-ref" (λ () (rope-ref string-sig empty-rope 0)))
        (check-exn #rx"bytes-ref" (λ () (rope-ref bytes-sig empty-rope 0)))
        (check-exn #rx"vector-ref" (λ () (rope-ref vector-sig empty-rope 0))))))

  ;; =====================================================================
  ;; Chunk ownership: mutable chunks alias the caller's data
  ;; =====================================================================
  (define suite-aliasing
    (test-suite "chunk ownership"
      (test-case "a vector chunk at or below the limit is stored verbatim, so it aliases"
        ;; chunk->rope reuses a chunk that fits. Strings and bytes are immutable and cannot
        ;; change; vectors are mutable, so a rope built from one follows the vector.
        (define v (vect 5))
        (define ρ (chunk->rope vector-sig v))
        (check-eq? (leaf-chunk ρ) v)
        (vector-set! v 0 'mutated)
        (check-eq? (rope-ref vector-sig ρ 0) 'mutated))
      (test-case "a split chunk is a copy, so mutation cannot leak into the rope"
        (define v (vect 130))
        (define ρ (chunk->rope vector-sig v))
        (check-false (eq? (leaf-chunk (node-left ρ)) v))
        (vector-set! v 0 'changed)
        ;; both halves are slices, so the rope keeps the original values
        (check-equal? (rope-length ρ) 130)
        (check-equal? (rope-ref vector-sig ρ 0) (elem-at 0))
        (check-equal? (rope-ref vector-sig ρ 1) (elem-at 1)))
      (test-case "immutable chunks cannot change, so a verbatim leaf is safe"
        (define s (str 100))
        (define ρ (string->rope s))
        (check-eq? (leaf-chunk ρ) s)
        (check-equal? (rope->string ρ) s)
        (define b (byt 100))
        (check-eq? (leaf-chunk (bytes->rope b)) b)
        (check-equal? (rope->bytes (bytes->rope b)) b))))

  ;; =====================================================================
  ;; Randomized invariants, for all three sigs
  ;; =====================================================================
  (define suite-randomized
    (test-suite "randomized invariants"
      (random-seed 20261028)
      (test-case "construction matches the source for random sizes"
        (for ([σ (in-list sigs)] [limit (in-list limits)])
          (for ([n (in-range 0 1500 7)])
            (define ρ (build σ n))
            (check-true (rope-valid? σ ρ))
            (check-true (<= (largest-chunk σ ρ) limit))
            (check-equal? (elems σ ρ) (elem-list σ (src σ n))))))
      (test-case "split and concat round-trip for random sizes"
        (for ([σ (in-list sigs)])
          (for ([n (in-range 0 900 11)])
            (define s (src σ n))
            (define ρ (build σ n))
            (define i (random (add1 n)))
            (define-values (a b) (rope-split σ ρ i))
            (check-true (rope-valid? σ a))
            (check-true (rope-valid? σ b))
            (check-equal? (elems σ (rope-concat σ a b)) (elem-list σ s)))))
      (test-case "slice and splice follow the chunk-level model"
        (for ([σ (in-list sigs)])
          (for ([n (in-range 1 700 13)])
            (define s (src σ n))
            (define ρ (build σ n))
            (define i (random n))
            (define j (+ i (random (add1 (- n i)))))
            (define sub (rope-slice σ ρ i j))
            (check-true (rope-valid? σ sub))
            (check-equal? (elems σ sub) (elem-list σ (slice* σ s i j)))
            (define ins-n (random 20))
            (define ins-chunk (src σ ins-n))
            (define ins (build σ ins-n))
            (define ρ* (rope-splice σ ρ i j ins))
            (check-true (rope-valid? σ ρ*))
            (check-equal? (elems σ ρ*) (elem-list σ (splice* σ s i j ins-chunk))))))
      (test-case "concat of two ropes keeps balance and content"
        (for ([σ (in-list sigs)])
          (for ([m (in-range 0 600 9)])
            (for ([k (in-range 0 600 17)])
              (define c (rope-concat σ (build σ m) (build σ k)))
              (check-true (rope-valid? σ c))
              (check-equal? (elems σ c)
                            (elem-list σ (append* σ (src σ m) (src σ k))))))))
      (test-case "string and bytes ropes flatten after random edits"
        (for ([n (in-range 100 1200 19)])
          (define s (str n))
          (define i (random n))
          (define j (+ i (random (add1 (- n i)))))
          (check-equal? (rope->string (rope-slice string-sig (string->rope s) i j))
                        (slice* string-sig s i j))
          (define b (byt n))
          (define ρb (bytes->rope b))
          (check-equal? (rope->bytes (rope-slice bytes-sig ρb i j))
                        (slice* bytes-sig b i j))
          (define ρ (string->rope s))
          (check-equal? (rope->string (rope-concat string-sig ρ ρ))
                        (append* string-sig s s))))))

  ;; =====================================================================
  ;; Run everything
  ;; =====================================================================
  (run-tests
    (test-suite "sigs"
      suite-surface
      suite-algebra
      suite-construction
      suite-flatten
      suite-operations
      suite-aliasing
      suite-randomized)))
