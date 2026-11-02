#lang racket/base

;;; benchmarks/limit-harness.rkt -- shared harness for the #:limit / #:merge-limit study.
;;;
;;; ADR-019 makes both limits tunables whose values must come from measurements. A limit
;;; pulls three ways at once: a bigger limit means a shallower tree (fewer leaves), a bigger
;;; worst-case copy per edit, and fewer but larger allocations per edit. The harness measures
;;; what each of those costs, per configuration:
;;;
;;;   latency      -- p50, p99, p99.9, max, plus a no-GC subset (ADR-021 splits the tail)
;;;   copy volume  -- bytes allocated per operation, exact
;;;   tree shape   -- leaf count, node count, height
;;;   retained     -- real resident bytes, read from /proc/self/status
;;;
;;; The wall-clock timer's resolution makes a latency at or near it noise, so copy volume is
;;; exact and reported beside the latency (racket-facts.md §1). `sample-batch` times a whole
;;; run for operations too small to time one at a time.

(require data/rope/core
         racket/file
         racket/list
         racket/math
         racket/string
         racket/vector)

(provide (all-defined-out))

;; ---------------------------------------------------------------------------------------
;; Token elements -- vector chunks of immutable token structs (notes/m0.org §3.1 Q4)
;; ---------------------------------------------------------------------------------------
;;
;; Token ropes are vector chunks of immutable token structs, each carrying its own relative
;; span; `width` is a chunk procedure summing those spans, not a constant per element. The
;; struct is deliberately three immediate fields -- kind tag, char width, relative span -- so
;; the measured cost is what the pipeline's token would actually cost. It lives in the
;; benchmark, not in data/rope.
(struct token (kind width span))

;; Build the deterministic token for index `i`, mirroring notes/m0.org §3.1 Q3: char widths
;; of 5-10, a span that advances past one separator, a small kind tag. Same multiplicative
;; step as the pseudo-offsets, so token text and edit positions are reproducible without a
;; mutable PRNG.
(define (token-of i)
  (define w (+ 5 (modulo (+ (* i 7919) 13) 6)))
  (token (modulo (+ (* i 31) 7) 8) w (+ w (+ 1 (modulo (* i 13) 2)))))

;; Return the sum of a chunk's token relative spans -- the width procedure token ropes use.
(define (token-chunk-width c)
  (for/fold ([w 0]) ([t (in-vector c)]) (+ w (token-span t))))

;; Build a deterministic token vector of `len` tokens starting at index `start`.
(define (token-vector start len)
  (build-vector len (λ (i) (token-of (+ start i)))))

;; Build a `chunk-dict` with caller-chosen limits. `elem` selects the chunk representation:
;; 'string for `string-dict`, 'bytes for `bytes-dict`, 'token for the vector-of-tokens
;; dict above.
(define (make-dict #:limit limit #:merge-limit merge-limit #:elem [elem 'string])
  (cond [(eq? elem 'string)
         (make-chunk-dict #:empty "" #:length string-length #:ref string-ref
                         #:slice substring #:append string-append
                         #:width 1 #:limit limit #:merge-limit merge-limit)]
        [(eq? elem 'token)
         (make-chunk-dict #:empty (vector) #:length vector-length #:ref vector-ref
                         #:slice vector-copy #:append vector-append
                         #:width token-chunk-width #:limit limit #:merge-limit merge-limit)]
        [else
         (make-chunk-dict #:empty #"" #:length bytes-length #:ref bytes-ref
                         #:slice subbytes #:append bytes-append
                         #:width 1 #:limit limit #:merge-limit merge-limit)]))

;; Return the element type `σ` works over, for choosing the document text.
(define (dict-elem σ)
  (cond [(vector? (chunk-dict-empty σ)) 'token]
        [(string? (chunk-dict-empty σ)) 'string]
        [else 'bytes]))

;; ---------------------------------------------------------------------------------------
;; Deterministic document text
;; ---------------------------------------------------------------------------------------

;; Keyword pool, so the generated text looks like source code rather than random bytes.
(define keyword-list '(lambda define let-values for/fold cond else when require provide))

;; Return one line of plausible code: indentation, a keyword, an identifier, padding, a
;; newline.
(define (code-line row)
  (define kw (list-ref keyword-list (modulo row (length keyword-list))))
  (define name (list->string (for/list ([i (+ 2 (modulo row 8))])
                               (integer->char (+ 97 (modulo (* row (+ i 3)) 26))))))
  (define indent (* (modulo row 4) 2))
  (string-append (if (zero? (modulo row 7)) ";" " ")
                 (string-repeat indent #\space)
                 kw " " name "() "
                 (string-repeat (- 42 (string-length kw)) #\space)))

;; Return the deterministic document character for index `i`: code text with a line break
;; every 40 columns.
(define (code-char i) (if (zero? (modulo i 40)) #\newline (integer->char (+ 32 (modulo i 90)))))

(define (string-repeat n c) (make-string (max n 0) c))

;; ---------------------------------------------------------------------------------------
;; Document text builders -- bounded runs over the deterministic character generator
;; ---------------------------------------------------------------------------------------

;; Return the document text from `start` for `len` characters, built in bounded runs so a
;; large buffer never holds an intermediate list of characters.
(define (code-string start len)
  (define chunk 4096)
  (string-append*
    (for/list ([i (in-range 0 len chunk)])
      (define k (min chunk (- len i)))
      (list->string (for/list ([j k]) (code-char (+ start i j)))))))

;; Return the element type `σ` works over -- an alias for `dict-elem`, the name the loading
;; helpers call.
(define (elem-of σ) (dict-elem σ))

;; ---------------------------------------------------------------------------------------
;; Loading documents -- piece-wise runs and one whole-buffer load
;; ---------------------------------------------------------------------------------------

;; The piece size an editor reads a file in -- independent of the leaf limit, so how many
;; runs a load touches is a separate metric from the whole-buffer chunk->rope case.
(define doc-piece 65536)

;; Return one deterministic run of document text, as a chunk of `σ`'s element type.
(define (doc-run σ n row #:piece [piece doc-piece])
  (define len (max 1 (min piece (- n (* row piece)))))
  (define s (code-string (* row piece) len))
  (if (eq? 'bytes (elem-of σ)) (string->bytes/utf-8 s) s))

;; Load a rope of `n` elements by appending runs of `piece` elements, as a file port would.
;; Return (values rope pieces-loaded). Each run dies right after it is consumed, so the heap
;; ends up holding only the rope.
(define (load-doc σ n #:piece [piece doc-piece])
  (define nrows (quotient (+ n (sub1 piece)) piece))
  (let fold ([ρ empty-rope] [row 0])
    (cond [(= row nrows) (values ρ row)]
          [else (fold (rope-concat σ ρ (chunk->rope σ (doc-run σ n row #:piece piece)))
                      (add1 row))])))

;; Load the whole buffer as one chunk of `n` elements, split into bounded leaves by
;; `chunk->rope`.
(define (buffer-load σ n)
  (define whole (code-string 0 n))
  (chunk->rope σ (if (eq? 'bytes (elem-of σ)) (string->bytes/utf-8 whole) whole)))

;; ---------------------------------------------------------------------------------------
;; Loading token documents -- piece-wise runs and one whole-buffer load
;; ---------------------------------------------------------------------------------------

;; The piece an incremental lexer reads a file in, in tokens -- independent of the leaf
;; limit, like doc-piece is for characters.
(define token-piece 4096)

;; Load a rope of `n` tokens by appending runs of `piece` tokens, as a file port would.
;; Return (values rope pieces-loaded); each run dies right after it is consumed.
(define (load-token-doc σ n #:piece [piece token-piece])
  (define nrows (quotient (+ n (sub1 piece)) piece))
  (let fold ([ρ empty-rope] [row 0])
    (cond [(= row nrows) (values ρ row)]
          [else (fold (rope-concat σ ρ
                                   (chunk->rope σ (token-vector (* row piece)
                                                  (max 1 (min piece (- n (* row piece)))))))
                      (add1 row))])))

;; Load the whole token buffer as one vector of `n` tokens, split into bounded leaves by
;; `chunk->rope`.
(define (buffer-load-tokens σ n)
  (chunk->rope σ (token-vector 0 n)))

;; ---------------------------------------------------------------------------------------
;; Timers -- the clocks every scenario reads
;; ---------------------------------------------------------------------------------------

;; Return the monotonic wall clock in milliseconds as a flonum. It counts from the process's
;; own start, so its magnitude stays small and the flonum keeps full precision, unlike
;; `current-inexact-milliseconds` (racket-facts.md §1). Its resolution sets the window sizes
;; the sweep uses (benchmarks/limit-sweep.rkt).
(define (now-ms) (current-inexact-monotonic-milliseconds))

;; Return the milliseconds spent inside the garbage collector, total since startup.
;; Samples compare this before and after each operation to spot a straddling collection
;; (ADR-021).
(define (gc-ms) (current-gc-milliseconds))

;; Return the bytes allocated since startup. Chunk copies of strings and bytes count exactly;
;; struct and cons allocation undercounts (racket-facts.md §2), so copy volume is reported
;; beside latency rather than inferred from it.
(define (mem-b) (current-memory-use))

;; ---------------------------------------------------------------------------------------
;; Latency samples -- percentiles split by whether a sample straddled a collection
;; ---------------------------------------------------------------------------------------
;;
;; The plain fields mix collector time into the tail; the `ng` fields carry the algorithm's
;; own tail (ADR-021).
;;
;; One measurement of one scenario. Latencies are per-operation nanoseconds (flonums);
;; counts are exact. Fields:
;;
;;   n          -- Operations sampled.
;;   mean       -- Mean latency.
;;   p50        -- Median latency.
;;   p99        -- 99th percentile latency.
;;   p999       -- 99.9th percentile latency.
;;   max        -- Largest latency.
;;   p50ng      -- Median over samples that did not straddle a collection.
;;   p99ng      -- 99th percentile over those samples.
;;   p999ng     -- 99.9th percentile over those samples.
;;   maxng      -- Largest latency over those samples.
;;   gc-ops     -- Samples or windows that straddled a collection.
;;   gc-time-ms -- Collector time observed while sampling.
;;   copy-mean  -- Mean bytes allocated per operation.
;;   copy-max   -- Largest allocation delta observed.
;;   copy-total -- Total bytes allocated while sampling.
(struct latency (n mean p50 p99 p999 max p50ng p99ng p999ng maxng gc-ops gc-time-ms
                 copy-mean copy-max copy-total)
  #:transparent)

;; Return the percentile of a sorted list of flonums. `p` is a fraction in (0, 1], and the
;; index is clamped to the last element, so an empty list returns +inf.0.
(define (percentile sorted-ns p)
  (define n (length sorted-ns))
  (if (zero? n)
      +inf.0
      (list-ref sorted-ns (min (- n 1) (exact-floor (* n p))))))

;; Sample `n` calls of `op` one at a time: latency, GC straddling, allocation bytes. `op` has
;; type (σ ρ i -> ρ), where `i` is the sample index and `op` threads the rope being edited.
(define (sample-per-op σ ρ op n #:collect-before [collect-before? #t])
  (if collect-before? (collect-garbage) (void))
  (define g0 (gc-ms))
  (let loop ([i 0] [ρ ρ] [ns null] [clean null] [copy null])
    (cond [(= i n)
           (define sorted (sort ns <))
           (define clean-sorted (sort clean <))
           (define copies (sort copy <))
           (latency n
                    (/ (apply + ns) n)
                    (percentile sorted .5)
                    (percentile sorted .99)
                    (percentile sorted .999)
                    (percentile sorted 1.0)
                    (percentile clean-sorted .5)
                    (percentile clean-sorted .99)
                    (percentile clean-sorted .999)
                    (percentile clean-sorted 1.0)
                    (- n (length clean))
                    (- (gc-ms) g0)
                    (/ (apply + copy) n)
                    (percentile copies 1.0)
                    (apply + copy))]
          [else
           (define t0 (now-ms))
           (define g0* (gc-ms))
           (define b0* (mem-b))
           (define ρ* (op σ ρ i))
           (define dt (* (- (now-ms) t0) 1000000.0))
           (define b1 (mem-b))
           (loop (add1 i) ρ*
                 (cons dt ns)
                 (if (= (gc-ms) g0*) (cons dt clean) clean)
                 (cons (- b1 b0*) copy))])))

;; Sample `n` calls as one run, reporting the mean per call. Percentile fields carry the
;; mean, because a batch cannot locate an individual call's tail; GC time and allocation
;; bytes stay exact.
(define (sample-batch σ ρ op n #:collect-before [collect-before? #t])
  (if collect-before? (collect-garbage) (void))
  (define g0 (gc-ms))
  (define b0 (mem-b))
  (define t0 (now-ms))
  (let loop ([i 0] [ρ ρ])
    (if (= i n)
        (let* ([total-ms (- (now-ms) t0)]
               [mean-ns (/ (* total-ms 1000000.0) n)]
               [copy-b (- (mem-b) b0)])
          (latency n mean-ns mean-ns mean-ns mean-ns mean-ns
                   mean-ns mean-ns mean-ns mean-ns
                   0 (- (gc-ms) g0) (/ copy-b n) (/ copy-b n) copy-b))
        (loop (add1 i) (op σ ρ i)))))

;; Sample `reps` windows of `window` operations, for operations whose latency is below the
;; timer's resolution. Each window runs `window` operations, and the percentiles are taken
;; over each window's mean per operation: a tail -- a collection or a maximum-size copy --
;; shows up as an inflated window, which attributes the tail to the GC rather than hiding
;; it. Fields:
;;
;;   n         -- Operations sampled, `reps` × `window`.
;;   gc-ops    -- Windows that straddled a collection.
;;   copy-mean -- Bytes allocated per operation, averaged over all windows.
(define (sample-windows σ ρ op #:window window #:reps reps #:collect-before [collect-before? #t])
  (if collect-before? (collect-garbage) (void))
  (define g0 (gc-ms))
  (let loop ([w 0] [ρ ρ] [means null] [clean null] [copy null])
    (if (= w reps)
        (let* ([total-ops (* reps window)]
               [sum-mean (apply + means)]
               [sorted (sort means <)]
               [clean-sorted (sort clean <)]
               [clean-n (length clean)])
          (latency total-ops
                   (/ sum-mean reps)
                   (percentile sorted .5)
                   (percentile sorted .99)
                   (percentile sorted .999)
                   (percentile sorted 1.0)
                   (percentile clean-sorted .5)
                   (percentile clean-sorted .99)
                   (percentile clean-sorted .999)
                   (percentile clean-sorted 1.0)
                   (- reps clean-n)
                   (- (gc-ms) g0)
                   (/ (apply + copy) total-ops)
                   (percentile (sort copy <) 1.0)
                   (apply + copy)))
        (let* ([base (* w window)]
               [t0 (now-ms)]
               [g0w (gc-ms)]
               [b0 (mem-b)]
               [ρ* (let inner ([k 0] [ρ ρ])
                     (if (= k window) ρ (inner (add1 k) (op σ ρ (+ base k)))))]
               [dt (* (- (now-ms) t0) 1000000.0)]
               [b1 (mem-b)])
          (loop (add1 w) ρ*
                (cons (/ dt window) means)
                (if (= (gc-ms) g0w) (cons (/ dt window) clean) clean)
                (cons (- b1 b0) copy))))))

;; ---------------------------------------------------------------------------------------
;; Resident memory -- read from the process status, not from the heap
;; ---------------------------------------------------------------------------------------

;; Return the value of a `/proc/self/status` field, in kilobytes. procfs files are not
;; readable with `file->lines`, so the whole file is read as bytes and the field is
;; matched with a string regexp (racket-facts.md §2).
(define (status-kb name)
  (define s (file->bytes "/proc/self/status"))
  (define m (regexp-match (format "~a[ \\t]*([0-9]+)" name) s))
  (cond [m (string->number (list->string (map integer->char (bytes->list (cadr m)))))]
        [else 0]))

(define (rss-kb) (status-kb "VmRSS:"))
(define (peak-kb) (status-kb "VmHWM:"))

;; Return the resident bytes held by the value `thunk` returns, measured while it is live.
;; A collection is forced first so the baseline is the heap already reachable, not
;; allocation slop.
(define (live-rss thunk)
  (collect-garbage)
  (define a (rss-kb))
  (define keep (thunk))
  (define b (rss-kb))
  (void keep)
  (- b a))

;; Return the resident bytes gained by `thunk` without forcing a collection first -- the peak
;; the scenario's own intermediates push the process to.
(define (delta-rss thunk)
  (define a (rss-kb))
  (define keep (thunk))
  (define b (rss-kb))
  (void keep)
  (- b a))

