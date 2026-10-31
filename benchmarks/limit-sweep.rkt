#lang racket/base

;;; benchmarks/limit-sweep.rkt -- the measurement that decides string-sig's and bytes-sig's
;;; #:limit and #:merge-limit (ADR-019, notes.org §3.2 Q8 and Q11), and now the token vector-sig's
;;; limits too (ADR-019's token revisit clause).
;;;
;;; The winner, recorded here so a rerun can be compared against it: string-sig 512/64 (limit
;;; 512, merge-limit 64) and bytes-sig 2048/512. Both declare a merge-limit well below the limit,
;;; which is what makes typing and patching cheap; the read path pays roughly 10% over its best.
;;; vector-sig keeps the default merge-limit == limit because nothing measures it -- that gap is
;;; what elem=token below closes (notes.org §3.1 Q4: token ropes are vector chunks of immutable
;;; token structs carrying their own relative spans; the token-sig is built in limit-harness.rkt,
;;; NOT in data/rope, because the pipeline type ships later).
;;;
;;; The workload is a programming editor's incremental parser pipeline: a buffer is loaded,
;;; edited by typing and by 4-for-4 patches, and read back by the lexer -- per-token slices,
;;; chunk-wise scans, random offset access, and a whole-buffer flatten for the full re-parse.
;;; Everything a limit changes shows up in one of the four metrics this sweep prints: latency,
;;; copy bytes, tree shape, resident memory.
;;;
;;; With elem=token the ladder is re-based per §3.1 Q3 (R4): token counts of 140 / 1400 / 7000 /
;;; 30000 / 70000, i.e. ~1-10 kilochars typical (~140-1400 tokens at 5-10 chars/token), 20-50 K
;;; chars large (~7000 tokens), a few hundred kilochars at stress (~30-70 K tokens). Limits sweep
;;; 32/64/128/256/512 with merge-limit at limit/8, limit/4, limit/2, and limit. These token
;;; values are the bare-run defaults; text runs pass them explicitly.
;;;
;;; Run it as:
;;;   raco make benchmarks/limit-harness.rkt benchmarks/limit-sweep.rkt
;;;   racket benchmarks/limit-sweep.rkt elem=token
;;;   racket benchmarks/limit-sweep.rkt elem=string sizes=1000000,10000000 \
;;;     limits=256,512,1024,2048,4096,8192,16384 merges=limit,limit/2,limit/4
;;;
;;; Configuration grammar is `key=value` arguments (see `parse-args` below).

(require data/rope/core
         data/rope/sigs
         racket/cmdline
         racket/list
         racket/format
         racket/string
         "limit-harness.rkt")

;; ---------------------------------------------------------------------------------------
;; Workload operations
;; ---------------------------------------------------------------------------------------

;; A deterministic offset: a multiplicative step, no PRNG object, no mutable state.
(define (pseudo-offset i len) (modulo (+ (* i 7919) 13) (max 1 len)))

;; A chunk whose element type matches the signature: strings for string-sig, bytes for bytes-sig.
(define (chunk-of σ str)
  (if (eq? 'bytes (elem-of σ)) (string->bytes/utf-8 str) str))

;; A keystroke: insert one character at the cursor, which advances through the buffer. The
;; cursor sits mid-buffer, so inserts land inside leaves and merge boundary leaves when they
;; can -- the behavior `merge-limit` decides.
(define (op-type σ ρ i)
  (define len (rope-length ρ))
  (define pos (min (+ (quotient len 2) i) (sub1 len)))
  (rope-splice σ ρ pos pos (chunk->rope σ (chunk-of σ "a"))))

;; A 4-for-4 patch: replace four characters at a pseudo-random position. The pseudo-offset can
;; land within the rope's last three elements, where pos+4 would pass the end, so the offset is
;; clamped to the last complete 4-element window.
(define (op-edit4-4 σ ρ i)
  (define len (rope-length ρ))
  (define pos (min (pseudo-offset i len) (max 0 (- len 4))))
  (rope-splice σ ρ pos (+ pos 4) (chunk->rope σ (chunk-of σ "abcd"))))

(define (op-delete1 σ ρ i)
  (define pos (pseudo-offset i (rope-length ρ)))
  (rope-splice σ ρ pos (+ pos 1) empty-rope))

;; A paste of an indent block: an insertion longer than one element.
(define (op-indent σ ρ i)
  (define pos (pseudo-offset i (rope-length ρ)))
  (rope-splice σ ρ pos pos (chunk->rope σ (chunk-of σ (string-repeat 30 #\space)))))

;; A token slice: the lexer cuts a short span out of the buffer as a rope of its own.
(define (op-token-slice σ ρ i)
  (define len (rope-length ρ))
  (define pos (pseudo-offset i (sub1 len)))
  (rope-slice σ ρ pos (min len (+ pos (+ 2 (modulo i 20))))))

;; A lexer pass over one region: fold the chunks of a 50 KB slice, counting elements. The rope
;; is unchanged, so the sampler keeps the same buffer for every sample.
(define (op-lex-scan σ ρ i)
  (define len (rope-length ρ))
  (define pos (pseudo-offset i (max 1 (- len 50000))))
  (define region (rope-slice σ ρ pos (+ pos (min 50000 (- len pos)))))
  (rope-fold-chunks (λ (acc c) (+ acc ((chunk-sig-length σ) c))) 0 region)
  ρ)

;; Random offset access: one element read at a pseudo-random index.
(define (op-random-ref σ ρ i)
  (rope-ref σ ρ (pseudo-offset i (rope-length ρ)))
  ρ)

;; Sequential access: a run of consecutive element reads.
(define (op-seq-ref σ ρ i)
  (define len (rope-length ρ))
  (define start (quotient len 2))
  (for ([k 1000] #:when (< (+ start k) len))
    (rope-ref σ ρ (+ start k)))
  ρ)

;; Whole-buffer serialization: the full re-parse or a buffer save.
(define (op-flatten σ ρ i)
  (if (eq? 'bytes (elem-of σ)) (rope->bytes ρ) (rope->string ρ))
  ρ)

;; ---------------------------------------------------------------------------------------
;; Token-pipeline operations (notes.org §3.1 Q4: vector chunks of token structs)
;; ---------------------------------------------------------------------------------------

;; Typing at the token level: replace one token at a pseudo-random position. Replacement --
;; not insertion -- keeps the token count stable across a run, so every configuration is edited
;; on a constant-size buffer; an insertion would grow the buffer one token per sample and let
;; a large `limit` drift into a different tree shape mid-run.
(define (op-token-type σ ρ i)
  (define len (rope-length ρ))
  (define pos (pseudo-offset i len))
  (rope-splice σ ρ pos (+ pos 1) (chunk->rope σ (vector (token-of (+ i 1000000)))))
  )

;; A 4-for-4 patch: replace four tokens at a pseudo-random position. As in op-edit4-4, the
;; pseudo-offset can land within the rope's last three tokens, so it is clamped to the last
;; complete 4-token window.
(define (op-token-patch σ ρ i)
  (define len (rope-length ρ))
  (define pos (min (pseudo-offset i len) (max 0 (- len 4))))
  (rope-splice σ ρ pos (+ pos 4) (chunk->rope σ (token-vector (* i 4) 4)))
  )

;; A lexer slice: cut a short window of tokens out of the buffer. The rope is unchanged, so the
;; sampler keeps the full buffer for every sample (the slice result is discarded); unlike the
;; text op-token-slice, this does not thread the slice as the new buffer.
(define (op-token-window-slice σ ρ i)
  (define len (rope-length ρ))
  (define pos (pseudo-offset i (sub1 len)))
  (rope-slice σ ρ pos (min len (+ pos (+ 2 (modulo i 10)))))
  ρ)

;; Random token access: one token read at a pseudo-random index.
(define (op-token-randref σ ρ i)
  (rope-ref σ ρ (pseudo-offset i (rope-length ρ)))
  ρ)

;; Sequential access: a run of consecutive token reads -- a lexer line's worth.
(define (op-token-seqref σ ρ i)
  (define len (rope-length ρ))
  (define start (quotient len 2))
  (for ([k 100] #:when (< (+ start k) len))
    (rope-ref σ ρ (+ start k)))
  ρ)

;; Whole-rope serialization: the full re-parse. Fold the chunks into one contiguous vector,
;; as rope->string does for text; the result is discarded and the rope stays threaded.
(define (op-token-flatten σ ρ i)
  (define out (make-vector (rope-length ρ)))
  (rope-fold-chunks (λ (k c) (vector-copy! out k c) (+ k (vector-length c))) 0 ρ)
  ρ)

;; ---------------------------------------------------------------------------------------
;; Scenario table
;; ---------------------------------------------------------------------------------------

;; One scenario: id, human label, the operation, how it is timed, and how much work it runs.
;;   'window   windowed sampling: `window` operations per sample, `reps` samples
;;   'batch    one run of `reps` calls, reporting the mean per call
;;   'per-op   samples each call separately, reporting percentiles of individual calls
(struct scenario (id label op mode reps window) #:transparent)

;; Sample counts. `reps` is multiplied by `scale`, which keeps the sweep short when a run is only
;; meant to read magnitudes and long when it is meant to produce a decision. Window sizes are
;; chosen so a window lasts several timer ticks: 32 elements at ~1 µs each is ~32 µs, far above
;; the 0.25 µs resolution, so a window's mean-per-operation is meaningful.
;; Windowed scenarios run 1024 windows: `percentile` clamps its index to n-1, so a p99.9 below
;; max only exists once n > 1000. At 20 windows p99, p99.9, and max were all the same sample.
(define scenario-list
  (list (scenario 'load    "load buffer (chunk->rope)"
                  (λ (σ ρ i) (buffer-load σ (rope-length ρ))) 'batch 1 0)
        (scenario 'loadp   "load file in pieces"
                  (λ (σ ρ i) (let-values ([(r _) (load-doc σ (rope-length ρ))]) r)) 'batch 1 0)
        (scenario 'type    "typing: insert 1 char"         op-type        'window 1024 32)
        (scenario 'edit    "patches: 4-for-4"             op-edit4-4     'window 1024 32)
        (scenario 'delete  "backspace: delete 1 char"     op-delete1     'window 1024 32)
        (scenario 'indent  "paste: 30-char block"         op-indent      'window 1024 32)
        (scenario 'slice   "lexer: token slices"          op-token-slice 'window 1024 32)
        (scenario 'randref "random element access"        op-random-ref  'window 1024 64)
        (scenario 'scan    "lexer: chunk scan of 50 KB"   op-lex-scan    'batch  12 0)
        (scenario 'seqref  "sequential element access"    op-seq-ref     'batch  20 0)
        (scenario 'flat    "flatten whole buffer"         op-flatten     'batch  3 0)))

;; Token-pipeline scenarios (ADR-019's token revisit): the same shapes as the editor pipeline
;; at token granularity. Windowed scenarios run 1024 windows like the text ones; construction
;; and flatten are batched, as load and flatten are for text, with counts raised so a batch
;; total stays above the timer's 0.25 µs resolution on the smallest sizes.
(define token-scenario-list
  (list (scenario 'tload    "load tokens (chunk->rope)"
                  (λ (σ ρ i) (buffer-load-tokens σ (rope-length ρ))) 'batch 5 0)
        (scenario 'tloadp   "load tokens in pieces"
                  (λ (σ ρ i) (let-values ([(r _) (load-token-doc σ (rope-length ρ))]) r)) 'batch 5 0)
        (scenario 'tappend  "token edit: replace 1 token"  op-token-type    'window 1024 32)
        (scenario 'tpatch   "token patch: 4-for-4"         op-token-patch   'window 1024 32)
        (scenario 'tslice   "lexer: token slice window"    op-token-window-slice 'window 1024 32)
        (scenario 'trandref "random token access"          op-token-randref 'window 1024 64)
        (scenario 'tseqref  "sequential token access"      op-token-seqref  'batch  20 0)
        (scenario 'tflat    "flatten token rope"           op-token-flatten 'batch  10 0)))

;; ---------------------------------------------------------------------------------------
;; One configuration
;; ---------------------------------------------------------------------------------------

;; A measured run: the configuration, the scenario, the latency struct, and the tree shape.
(struct row (elem limit merge size id label n mean p50 p99 p999 max p50ng p99ng p999ng maxng gc-ops
             copy-mean copy-total leaves height rss-kb) #:transparent)

;; Runs every scenario for one (elem, limit, merge-limit) configuration at one document size.
;; The rope is loaded once and threaded through the scenarios; the load scenarios build their own
;; buffer so they measure the construction path rather than the shared one. For elem=token the
;; scenarios are the token-pipeline list and the document is a token vector ladder.
(define (run-config elem limit merge size #:scale scale #:scenarios scen-ids)
  (define σ (make-sig #:limit limit #:merge-limit merge #:elem elem))
  (define-values (ρ _)
    (if (eq? elem 'token) (load-token-doc σ size) (load-doc σ size)))
  (define scen (if (eq? elem 'token) token-scenario-list scenario-list))
  (define leaves (rope-leaf-count ρ))
  (define height (rope-height ρ))
  (define rss (live-rss (λ () ρ)))
  (for/list ([s (in-list scen)]
             #:when (or (null? scen-ids) (memq (scenario-id s) scen-ids)))
    ;; A windowed scenario runs reps × window operations; a batch runs reps calls.
    (define reps (max 1 (* scale (scenario-reps s))))
    (define l (cond [(eq? (scenario-mode s) 'batch)
                     (sample-batch σ ρ (scenario-op s) reps)]
                    [(eq? (scenario-mode s) 'window)
                     (sample-windows σ ρ (scenario-op s) #:window (scenario-window s) #:reps reps)]
                    [else
                     (sample-per-op σ ρ (scenario-op s) reps)]))
    (row elem limit merge size (scenario-id s) (scenario-label s)
         (latency-n l)
         (latency-mean l) (latency-p50 l) (latency-p99 l) (latency-p999 l) (latency-max l)
         (latency-p50ng l) (latency-p99ng l) (latency-p999ng l) (latency-maxng l)
         (latency-gc-ops l) (latency-copy-mean l) (latency-copy-total l)
         leaves height rss)))

;; ---------------------------------------------------------------------------------------
;; Reporting
;; ---------------------------------------------------------------------------------------

;; Analytic retained size of a rope, in bytes per element: chunk payload plus the tree over it.
;; The layout constants are measured -- 32 bytes per leaf (one header word plus three slots) and
;; 48 bytes per node (one header word plus five slots), calibrated with /proc/self/status deltas
;; over 4M structs -- and the node count is leaves-1, which is what `chunk->rope` builds. A
;; string chunk costs 4 bytes per element on the CS backend; a bytes chunk costs 1 byte. A token
;; chunk costs 8 bytes for the vector slot plus ~32 bytes for the immutable token struct (the
;; same 3-field layout as a leaf), so ~40 bytes per token plus the tree.
(define (retained-per-elem elem size leaves)
  (define payload (cond [(eq? elem 'bytes) size]
                        [(eq? elem 'token) (* size 40)]
                        [else (* size 4)]))
  (define tree (+ (* leaves 32) (* (max 0 (- leaves 1)) 48)))
  (exact->inexact (/ (+ payload tree) size)))

;; One row as a tab-separated line, so the output can be piped into a spreadsheet or re-parsed
;; by another script. Latencies are rounded to nanoseconds; copy bytes are exact totals. The
;; last two columns are the resident-kilobyte delta measured for the loaded rope and the
;; analytic bytes-per-element cost of the rope.
(define (row->tsv r)
  ;; Latencies are rounded to nanoseconds; copy bytes are exact totals. `maxlat` is the row's
  ;; largest sample, so the name does not shadow the `max` procedure used below.
  (define-values (elem limit merge size id label n mean p50 p99 p999 maxlat p50ng p99ng p999ng maxng
                  gc-ops copy-mean copy-total leaves height rss)
    (values (row-elem r) (row-limit r) (row-merge r) (row-size r) (row-id r) (row-label r)
            (row-n r) (round (row-mean r)) (round (row-p50 r)) (round (row-p99 r))
            (round (row-p999 r)) (round (row-max r)) (round (row-p50ng r)) (round (row-p99ng r))
            (round (row-p999ng r)) (round (row-maxng r)) (row-gc-ops r) (round (row-copy-mean r))
            (row-copy-total r) (row-leaves r) (row-height r) (row-rss-kb r)))
  ;; Analytic retained size: chunk payload plus the tree over it.
  (format "~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a"
          elem limit merge size id label n mean p50 p99 p999 maxlat
          p50ng p99ng p999ng maxng gc-ops copy-mean copy-total leaves height rss
          (+ (* leaves 32) (* (max 0 (- leaves 1)) 48))
          (retained-per-elem elem size leaves)))

(define (config-name r)
  (format "~a/~a/~a" (row-elem r) (row-limit r) (row-merge r)))

;; A ranking of the configurations for one scenario at one size. The sort key is the p99 over the
;; samples that did not straddle a garbage collection -- the algorithm's own tail, per ADR-021 --
;; then p50. GC-overlapping tails are reported alongside, not folded into the decision.
(define (print-ranking rows #:id id #:size size #:top k)
  (define here (filter (λ (r) (and (eq? (row-id r) id) (= (row-size r) size))) rows))
  (define (better? a b)
    (cond [(< (row-p99ng a) (row-p99ng b)) #t]
        [(> (row-p99ng a) (row-p99ng b)) #f]
        [else (< (row-p50 a) (row-p50 b))]))
  (define sorted (sort here better?))
  (if (null? sorted)
      (void)
      (begin
        (displayln (format "\n--- ~a at ~a elements, ranked by clean p99 then p50 ---" id size))
        (for ([r (in-list (take sorted (min k (length sorted))))])
          (displayln
           (format "  ~a  p50=~a p99=~a clean-p99=~a clean-max=~a max=~a gc-windows=~a copy/op=~a leaves=~a h=~a bytes/elem=~a"
                   (config-name r) (round (row-p50 r)) (round (row-p99 r))
                   (round (row-p99ng r)) (round (row-maxng r)) (round (row-max r))
                   (row-gc-ops r) (round (row-copy-mean r)) (row-leaves r) (row-height r)
                   (retained-per-elem (row-elem r) (row-size r) (row-leaves r)))))))
  (void))

(define (parse-args args)
  (for/fold ([opts '()]) ([a (in-list args)])
    (define parts (string-split a "="))
    (cond [(= (length parts) 2)
           (cons (cons (string->symbol (car parts)) (cadr parts)) opts)]
          [else opts])))

(define (opt opts key default)
  (define m (assq key opts))
  (if m (cdr m) default))

;; A list of numbers from a comma-separated string, skipping anything unparseable.
(define (numbers s)
  (for/list ([tok (in-list (string-split s ","))] #:when (string->number tok))
    (string->number tok)))

;; A merge-limit expression: "limit" means merge-limit = limit, "limit/2" means limit/2, and a
;; literal number stands on its own. Never above the leaf limit, which would break the leaf
;; invariant (notes.org §1.3, invariant 1).
(define (merge-of expr limit)
  (cond [(string=? expr "limit") limit]
        [(string=? expr "limit/2") (quotient limit 2)]
        [(string=? expr "limit/4") (quotient limit 4)]
        [(string=? expr "limit/8") (quotient limit 8)]
        [(string=? expr "min") 1]
        [else (max 1 (min limit (or (string->number expr) limit)))]))

;; ---------------------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------------------

(define (main args)
  (define opts (parse-args args))
  ;; Bare-run defaults are the token revisit ladder (§3.1 Q3, R4): token counts 140 / 1400 /
  ;; 7000 / 30000 / 70000 with limits 32..512 and merge-limit at limit/8, limit/4, limit/2,
  ;; limit. Text runs pass elem/sizes/limits/merges explicitly, as above.
  (define elems (map string->symbol (string-split (opt opts 'elem "token") ",")))
  (define sizes (numbers (opt opts 'sizes "140,1400,7000,30000,70000")))
  (define limits (numbers (opt opts 'limits "32,64,128,256,512")))
  (define merge-exprs (string-split (opt opts 'merges "limit/8,limit/4,limit/2,limit") ","))
  (define scen-ids (map string->symbol (string-split (opt opts 'scenarios "") ",")))
  (define scale (or (string->number (opt opts 'scale "1")) 1))
  (define top (or (string->number (opt opts 'top "5")) 5))

  (define rows
    (for*/list ([elem elems] [size sizes] [limit limits] [expr merge-exprs]
                [r (in-list (run-config elem limit (merge-of expr limit) size
                                      #:scale scale #:scenarios scen-ids))])
      r))

  ;; The header names the columns, so the output is self-describing when piped somewhere.
  (displayln (string-join
             (list "elem" "limit" "merge-limit" "size" "id" "label" "ops" "mean-ns" "p50-ns"
                   "p99-ns" "p99.9-ns" "max-ns" "p50-no-gc" "p99-no-gc" "p99.9-no-gc" "max-no-gc"
                   "gc-windows" "copy-mean-b" "copy-total-b" "leaves" "height" "rss-kb" "tree-b" "bytes/elem")
             "\t"))
  (for ([r (in-list rows)]) (displayln (row->tsv r)))
  (for ([id (remove-duplicates (map row-id rows))])
    (for ([size sizes]) (print-ranking rows #:id id #:size size #:top top)))
  (void))



;; Entry point: `racket benchmarks/limit-sweep.rkt key=value ...` runs the sweep.
(module+ main
  (command-line #:args args (main args)))
