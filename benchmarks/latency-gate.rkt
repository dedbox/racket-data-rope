#lang racket/base

;;; benchmarks/latency-gate.rkt -- the latency-regression gate over the pinned ceilings.
;;;
;;; === What the gate checks ===
;;;
;;; notes/m0.org §3.2 row 12 pins each shipped shape's budget: p50 and clean p99 latency
;;; per operation for `type` and `randref`, at the two sizes and configurations the row
;;; names. §1.6 requires the median of at least three runs, because one run's clean p99
;;; spreads wide enough at matched sizes that a lone run can read as a regression about
;;; one time in three (row 12 records the spread), so this file keeps no figures of its
;;; own. The gate runs `benchmarks/limit-sweep.rkt` that many times per shape, over the whole
;;; scenario list the ceilings were pinned on, at the pinned sizes, takes each cell's median,
;;; and exits non-zero when a median passes its ceiling.
;;;
;;; === The note grammar it reads ===
;;;
;;; Every number comes out of the note, so re-pinning a ceiling edits the note and nothing
;;; else. Row 12 states one configuration per shape and one block per size: a shape marker
;;; ("Bytes shape", "String shape"), the config code (`=bytes/2048/256=`), then a block led
;;; by "100 K typing" or "10 M typing" holding two `-> ceilings A / B ns` pairs in order
;;; `type` then `randref`. The sizes come out of those markers and the leaf limit out of the
;;; config code, the scenario ids decide which sweep rows become cells, and the merge limit
;;; is the shipped pairing `merge-limit` = `limit`/8, so every argument the sweep takes
;;; comes from the note and no constant table repeats it here. The gate asserts exactly
;;; 8 pairs -- 2 shapes x 2 sizes x 2 scenarios -- and fails loudly naming §3.2 row 12 when
;;; the grammar does not match.
;;;
;;; === Running it ===
;;;
;;;   raco make benchmarks/latency-gate.rkt benchmarks/limit-sweep.rkt
;;;   racket benchmarks/latency-gate.rkt          ; three runs per shape, §1.6's minimum
;;;   racket benchmarks/latency-gate.rkt runs=6   ; more runs against the same ceilings
;;;
;;; A gate is not a test: `raco test tests/` never runs it, and it sweeps `runs` times per
;;; shape, so a pass takes seconds on a quiet machine and minutes once the box is loaded. It
;;; prints one line per cell -- median, ceiling, margin for both metrics -- then PASS or
;;; BREACH, and measures only the two pinned shapes at the two pinned sizes.

(require racket/file
         racket/list
         racket/math
         racket/port
         racket/runtime-path
         racket/string
         racket/system)

;; ---------------------------------------------------------------------------------------
;; Values -- the ceilings, the configuration they were pinned at, and what a sweep reports
;; ---------------------------------------------------------------------------------------

;; One budget pinned by notes/m0.org §3.2 row 12 for one (shape, size, scenario) cell.
;; Fields:
;;
;;   shape   -- Shape the ceiling was measured on, which is the sweep's `elem=` argument.
;;   size    -- Elements per document the ceiling was measured at.
;;   id      -- Scenario id as the sweep prints it, `type` for typing, `randref` reads.
;;   p50     -- Budget for the median per-operation latency, in nanoseconds.
;;   p99ng   -- Budget for the p99 over samples that did not straddle a collection, in ns.
(struct ceiling (shape size id p50 p99ng) #:transparent)

;; The configuration a shape's ceilings were pinned at, as the row's config code states it.
;; Fields:
;;
;;   shape  -- Shape the note names in its marker, 'bytes or 'string.
;;   limit  -- Leaf limit, read from the config code that follows the marker.
;;   merge  -- Merge limit from the same code, checked against the `limit`/8 pairing.
(struct pin (shape limit merge) #:transparent)

;; One sweep row's gate-relevant fields, read by the sweep's own header column names.
;; Fields:
;;
;;   shape  -- Shape the sweep ran, taken from the `elem=` argument the gate passed it.
;;   id     -- Scenario id, read from the header's `id` column.
;;   size   -- Elements per document, read from the header's `size` column.
;;   p50    -- Median per-operation latency in ns, read from the `p50-ns` column.
;;   p99ng  -- Clean p99 in ns, read from the `p99-no-gc` column.
(struct sample (shape id size p50 p99ng) #:transparent)

;; One gate cell: a ceiling beside the medians of the runs that measured it. Fields:
;;
;;   ceiling -- Ceiling the cell compares against, read from the note.
;;   p50     -- Median p50 over the runs, rounded to whole nanoseconds.
;;   p99ng   -- Median clean p99 over the runs, rounded to whole nanoseconds.
;;   ok?     -- True when both medians sit at or below their budgets.
(struct cell (ceiling p50 p99ng ok?) #:transparent)

;; ---------------------------------------------------------------------------------------
;; Grammar -- the markers of §3.2 row 12, matched in a single pass
;; ---------------------------------------------------------------------------------------

;; The row-12 grammar as one alternation, matched in the order the row writes it: a shape
;; marker opens a shape's block, a size marker opens a size's block and names the scenario
;; that comes first, and each ceiling pair follows inside its block. Requiring a scenario
;; word after a size marker keeps prose that merely mentions "10 M" from opening a block.
;; Whitespace is spelled as `[ \t]`: this backend's regexp reads `[[:space:]]` as a literal
;; character set and matches nothing, so a POSIX class silently finds no markers at all.
(define row-12-grammar
  #rx"(Bytes shape|String shape|(100 K|10 M)[ \t]+(typing|randref)|-> ceilings ([0-9]+) / ([0-9]+) ns)")

;; The three kinds of marker above, matched one at a time to read the fields of a whole
;; match. `regexp-match*` hands back whole matches, so classification reads them here.
(define bytes-marker #rx"Bytes shape")
(define string-marker #rx"String shape")
(define size-marker #rx"(100 K|10 M)[ \t]+(typing|randref)")
(define ceiling-marker #rx"-> ceilings ([0-9]+) / ([0-9]+) ns")

;; The row's config code, `=bytes/2048/256=`, carrying shape, leaf limit and merge limit.
(define config-grammar #rx"=(bytes|string|vector)/([0-9]+)/([0-9]+)=")

;; Return the element count a size marker stands for, failing on a marker the row pins no
;; budget for. Only the two sizes row 12 measures are accepted here.
(define (size-of marker)
  (cond [(string=? marker "100 K") 100000]
        [(string=? marker "10 M") 10000000]
        [else (grammar-error (format "the size marker ~s carries no budget" marker))]))

;; Return the note's spelling of `size`, so a report line names the block that pinned it:
;; 100000 -> "100 K", 10000000 -> "10 M". A size the row does not use prints as a number.
(define (size-label size)
  (cond [(zero? (modulo size 1000000)) (format "~a M" (quotient size 1000000))]
        [(zero? (modulo size 1000)) (format "~a K" (quotient size 1000))]
        [else (number->string size)]))

;; ---------------------------------------------------------------------------------------
;; Failure messages -- what a mismatch says, and where to look for it
;; ---------------------------------------------------------------------------------------

;; Stop with a message naming notes/m0.org §3.2 row 12: every budget the gate reads comes
;; from that row, so a grammar mismatch points at the row rather than at a constant here.
(define (grammar-error what)
  (error 'latency-gate "~a -- the budgets live in notes/m0.org §3.2 row 12" what))

;; Stop on a failure of the run itself: a note that is not there, a sweep that died, or a
;; cell the sweep did not report.
(define (run-error what) (error 'latency-gate "~a" what))

;; ---------------------------------------------------------------------------------------
;; The note -- row 12 read into ceilings and configurations
;; ---------------------------------------------------------------------------------------

;; The note, resolved beside this file so the gate runs from any directory: the one source
;; of every budget the gate compares against.
(define-runtime-path note-path "../notes/m0.org")

;; Return the text of notes/m0.org.
(define (read-note)
  (unless (file-exists? note-path) (grammar-error (format "no note at ~a" note-path)))
  (file->string note-path))

;; Return the text of §3.2 row 12: the table row numbered 12 whose question text carries a
;; `-> ceilings` pair. Fail when the note holds no such row, or more than one.
(define (row-12 text)
  (define hits
    (for/list ([line (in-list (string-split text "\n"))]
               #:when (and (string-prefix? line "| 12 |") (regexp-match? #rx"-> ceilings" line)))
      line))
  (unless (= (length hits) 1)
    (grammar-error (format "expected one row 12, found ~a" (length hits))))
  (car hits))

;; Return the ceilings row 12 pins, in the order the note writes them. The fold walks the
;; markers in order, and a shape or size marker resets the pair counter for its block: the
;; counter decides which scenario a pair belongs to, the first pair being `type`.
(define (parse-ceilings row)
  (define-values [ceilings _shape _size _nth]
    (for/fold ([ceilings null] [shape #f] [size #f] [nth 0])
              ([whole (in-list (regexp-match* row-12-grammar row))])
      (cond
        [(regexp-match? bytes-marker whole) (values ceilings 'bytes size 0)]
        [(regexp-match? string-marker whole) (values ceilings 'string size 0)]
        ;; The row puts `type` first inside a size block, so that is what a marker names.
        [(regexp-match? size-marker whole)
         (define parts (regexp-match size-marker whole))
         (unless (equal? (caddr parts) "typing")
           (grammar-error "a size block opens with the randref scenario"))
         (values ceilings shape (size-of (cadr parts)) 0)]
        [(regexp-match? ceiling-marker whole)
         (unless (and shape size)
           (grammar-error "a ceiling pair sits before any shape or size marker"))
         (when (>= nth 2)
           (grammar-error "a size block holds more than two ceiling pairs"))
         (define parts (regexp-match ceiling-marker whole))
         (values (cons (ceiling shape size (if (zero? nth) 'type 'randref)
                         (string->number (cadr parts))
                         (string->number (caddr parts)))
                       ceilings)
                 shape size (add1 nth))]
        [else (values ceilings shape size nth)])))
  (reverse ceilings))

;; Return the same ceilings once they match the row's shape: exactly 8 pairs, one `type`
;; and one `randref` pair for every shape and size. Fail naming row 12 when they do not.
(define (assert-ceilings ceilings)
  (unless (= (length ceilings) 8)
    (grammar-error (format "the grammar matched ~a ceiling pairs, the row pins 8"
                           (length ceilings))))
  (define sizes (sort (remove-duplicates (map ceiling-size ceilings)) <))
  (unless (= (length sizes) 2)
    (grammar-error (format "the grammar pins ~a sizes, the row pins 2" (length sizes))))
  (for* ([shape (in-list '(bytes string))] [size (in-list sizes)])
    (define ids
      (map ceiling-id (filter (λ (c) (and (eq? (ceiling-shape c) shape)
                                          (= (ceiling-size c) size))) ceilings)))
    (unless (equal? ids '(type randref))
      (grammar-error (format "the ~a block at ~a holds ~a, the row pins type then randref"
                             shape size ids))))
  ceilings)

;; Return the configuration pinned for one shape marker: the first config code after it, so
;; a superseded pin quoted further down the row cannot be read as the one in force. Fail when
;; the marker or its config code is missing, when the code names another shape, or when its
;; merge limit breaks the shipped pairing -- `merge-limit` = `limit`/8 is ADR-019's rule and
;; notes/m0.org §3.2 row 8 settled it, so `run-sweep` passes `merges=limit/8` and the config
;; code read here has to agree with that pairing.
(define (parse-pin row marker shape)
  (define pos (regexp-match-positions marker row))
  (unless pos (grammar-error (format "no marker for the ~a shape" shape)))
  (define m (regexp-match config-grammar (string-trim (substring row (cdr (car pos))))))
  (unless m (grammar-error (format "no config code follows the ~a marker" shape)))
  (unless (equal? (cadr m) (symbol->string shape))
    (grammar-error (format "the config code names ~a, the marker names ~a" (cadr m) shape)))
  (define limit (string->number (list-ref m 2)))
  (define merge (string->number (list-ref m 3)))
  (unless (= merge (quotient limit 8))
    (grammar-error
      (format "the ~a config code pairs limit ~a with merge ~a, which is not limit/8"
              shape limit merge)))
  (pin shape limit merge))

;; ---------------------------------------------------------------------------------------
;; The sweep -- one child run per measurement, rows read by header column name
;; ---------------------------------------------------------------------------------------

;; Return the `racket` executable on the search path, so a child sweep runs on the same
;; backend as this file. `find-system-path 'run-file` names the =.zo= this file runs from
;; after `raco make`, and exec on it fails with EACCES, so the name is resolved directly.
(define (racket-exe)
  (define exe (find-executable-path "racket"))
  (unless exe (run-error "no racket executable on the search path"))
  exe)

;; The sweep beside this file, run as a child process rather than required, so its TSV
;; output is parsed the way any other consumer of it would parse it.
(define-runtime-path sweep-path "limit-sweep.rkt")

;; Return the samples one run of the sweep reports for configuration `p`, measured in a
;; child process. `sizes` come from the note's markers and the leaf limit from its config
;; code. The child runs the sweep's whole scenario list rather than only the two gated ids:
;; row 12's ceilings were read in that harness, and a comparison has to use the same one
;; (row 12's own caveat). `cell-for` reads the two ids the note pins out of the rows.
(define (run-sweep p sizes)
  (define args
    (cons sweep-path
          (list (format "elem=~a" (pin-shape p))
                (format "sizes=~a" (string-join (map number->string sizes) ","))
                (format "limits=~a" (pin-limit p))
                ;; ADR-019's pairing; `parse-pin` checks the note agrees with it.
                "merges=limit/8")))
  (define child (apply process* (cons (racket-exe) args)))
  ;; Drain both pipes before waiting: a child that fills one of them while the gate blocks
  ;; on the other never finishes.
  (define out (port->string (list-ref child 0)))
  (define err (port->string (list-ref child 3)))
  ((list-ref child 4) 'wait)
  (define code ((list-ref child 4) 'exit-code))
  (unless (zero? code)
    (run-error (format "the sweep for ~a exited with code ~a: ~a"
                       (pin-shape p) code (string-trim err))))
  (parse-output (pin-shape p) out))

;; Return column `i` of `fields` as a number, failing when the sweep wrote something else
;; in a column the gate reads.
(define (number-at fields i name)
  (define v (string->number (list-ref fields i)))
  (unless v (run-error (format "the sweep's ~a column is not a number" name)))
  v)

;; Return the samples in `out` for shape `shape`, keyed by the header's column names rather
;; than by position: `id`, `size`, `p50-ns` and `p99-no-gc`. Ranking blocks the sweep
;; prints after its rows lack the header's field count, so they are skipped.
(define (parse-output shape out)
  (define lines (string-split out "\n"))
  (unless (pair? lines) (run-error "the sweep printed no output"))
  (define cols (string-split (car lines) "\t"))
  (define (column name)
    (define i (index-of cols name))
    (unless i (run-error (format "the sweep header has no ~a column" name)))
    i)
  (define id-i (column "id"))
  (define size-i (column "size"))
  (define p50-i (column "p50-ns"))
  (define p99-i (column "p99-no-gc"))
  (for*/list ([line (in-list (cdr lines))]
              [fields (in-list (list (string-split line "\t")))]
              #:when (= (length fields) (length cols)))
    (sample shape
            (string->symbol (list-ref fields id-i))
            (number-at fields size-i "size")
            (number-at fields p50-i "p50-ns")
            (number-at fields p99-i "p99-no-gc"))))

;; ---------------------------------------------------------------------------------------
;; Cells -- the median of the runs against each ceiling
;; ---------------------------------------------------------------------------------------

;; Return the median of `ns`: the middle value for an odd count, the mean of the two middle
;; values for an even one. §1.6 needs at least three runs, so the odd path is the norm.
(define (median ns)
  (define sorted (sort ns <))
  (define n (length sorted))
  (unless (> n 0) (run-error "no samples to take a median of"))
  (cond [(odd? n) (list-ref sorted (quotient n 2))]
        [else (/ (+ (list-ref sorted (sub1 (quotient n 2)))
                    (list-ref sorted (quotient n 2))) 2)]))

;; Return the cell for one ceiling: the median p50 and the median clean p99 over the runs
;; that reported it, rounded to whole nanoseconds as the row's ceilings are. `runs` is how
;; many runs must have reported every ceiling, so a scenario the sweep skipped fails here
;; rather than showing up as an absent row.
(define (cell-for ceiling samples runs)
  (define here
    (filter (λ (s) (and (eq? (sample-shape s) (ceiling-shape ceiling))
                        (eq? (sample-id s) (ceiling-id ceiling))
                        (= (sample-size s) (ceiling-size ceiling))))
            samples))
  (unless (= (length here) runs)
    (run-error (format "~a of ~a runs reported the ~a cell at ~a elements"
                       (length here) runs (ceiling-id ceiling) (ceiling-size ceiling))))
  (define p50 (exact-round (median (map sample-p50 here))))
  (define p99ng (exact-round (median (map sample-p99ng here))))
  (cell ceiling p50 p99ng (and (<= p50 (ceiling-p50 ceiling))
                               (<= p99ng (ceiling-p99ng ceiling)))))

;; ---------------------------------------------------------------------------------------
;; Reporting -- one line per cell, then the overall verdict
;; ---------------------------------------------------------------------------------------

;; Return `s` padded on the right to `w` columns, so report lines line up.
(define (pad s w)
  (define n (string-length s))
  (if (>= n w) s (string-append s (make-string (- w n) #\space))))

;; The padded column names of one metric block, at the widths `metric-columns` uses below.
(define metric-header
  (list (pad "metric" 10) (pad "median" 8) (pad "ceiling" 8) (pad "margin" 8)))

;; Return a cell's comparisons as (name median ceiling) triples, p50 then clean p99.
(define (cell-metrics c)
  (list (list "p50" (cell-p50 c) (ceiling-p50 (cell-ceiling c)))
        (list "clean-p99" (cell-p99ng c) (ceiling-p99ng (cell-ceiling c)))))

;; Return `margin` signed: headroom reads +49, a breach reads -15.
(define (margin-string margin)
  (if (>= margin 0) (format "+~a" margin) (format "~a" margin)))

;; Return one comparison's columns: metric, median, ceiling, margin, each padded.
(define (metric-columns metric)
  (list (pad (car metric) 10)
        (pad (format "~a" (cadr metric)) 8)
        (pad (format "~a" (caddr metric)) 8)
        (pad (margin-string (- (caddr metric) (cadr metric))) 8)))

;; Print the report's column names, at the widths `pad` gives every line that follows.
(define (report-header)
  (displayln
    (string-join
      (append (list (pad "shape" 7) (pad "size" 6) (pad "scenario" 8))
              metric-header metric-header (list "verdict"))
      " ")))

;; Print one cell: its shape, size and scenario, then both metrics on the same line, each
;; with its median, its ceiling and the margin the median leaves.
(define (report-cell c)
  (define cl (cell-ceiling c))
  (displayln
    (string-join
      (append (list (pad (format "~a" (ceiling-shape cl)) 7)
                    (pad (size-label (ceiling-size cl)) 6)
                    (pad (format "~a" (ceiling-id cl)) 8))
              (metric-columns (car (cell-metrics c)))
              (metric-columns (second (cell-metrics c)))
              (list (if (cell-ok? c) "ok" "BREACH")))
      " ")))

;; Return the tightest margin over every metric of every cell: the smallest headroom, which
;; is negative when some median passed its budget.
(define (tightest-margin cells)
  (apply min (for*/list ([c (in-list cells)] [m (in-list (cell-metrics c))])
               (- (caddr m) (cadr m)))))

;; Return the report's last line: PASS with the tightest margin, or BREACH naming every
;; comparison that passed its budget and how many cells it covers.
(define (verdict cells runs)
  (define breached
    (for*/list ([c (in-list cells)] [m (in-list (cell-metrics c))]
                #:when (> (cadr m) (caddr m)))
      (format "~a ~a ~a ~a ~a over ~a" (ceiling-shape (cell-ceiling c))
              (size-label (ceiling-size (cell-ceiling c)))
              (ceiling-id (cell-ceiling c)) (car m) (cadr m) (caddr m))))
  (cond [(null? breached)
         (format "PASS -- ~a cells, median of ~a runs per shape, tightest margin ~a"
                 (length cells) runs (margin-string (tightest-margin cells)))]
        [else
         (define failed (filter (λ (c) (not (cell-ok? c))) cells))
         (format "BREACH -- ~a/~a comparisons over budget, ~a/~a cells, ~a runs each: ~a"
                 (length breached) (* 2 (length cells)) (length failed) (length cells)
                 runs (string-join breached "; "))]))

;; ---------------------------------------------------------------------------------------
;; Main -- the entry point, a gate rather than a test
;; ---------------------------------------------------------------------------------------

;; Resolve the configuration arguments the way the sweep does: `key=value` pairs, with a
;; later one winning over an earlier one.
(define (parse-args args)
  (for/fold ([opts '()]) ([a (in-list args)])
    (define parts (string-split a "="))
    (cond [(= (length parts) 2) (cons (cons (string->symbol (car parts)) (cadr parts)) opts)]
          [else opts])))

;; Return the value of `key` in `opts`, or `default` when no argument gives it.
(define (opt opts key default)
  (define m (assq key opts))
  (if m (cdr m) default))

;; Return the runs to take per shape, 3 by default: §1.6 reads the median of at least
;; three runs, so a smaller count is refused rather than trusted as a baseline.
(define (resolve-runs s)
  (define n (string->number s))
  (unless (and n (exact? n) (>= n 3))
    (run-error (format "runs must be a whole number of at least 3, got ~a" s)))
  n)

;; Return the samples of one run, announcing it on stderr: stdout stays the report and
;; stderr carries the progress, so the table reads cleanly when piped.
(define (run/announce p sizes run total)
  (eprintf "  ~a: sweep run ~a of ~a\n" (pin-shape p) run total)
  (run-sweep p sizes))

;; Run the sweep `runs` times per pinned shape, print one line per cell against the row's
;; ceilings, and exit non-zero when any median passes its budget.
(define (main args)
  (define opts (parse-args args))
  (define runs (resolve-runs (opt opts 'runs "3")))
  (define row (row-12 (read-note)))
  (define ceilings (assert-ceilings (parse-ceilings row)))
  (define pins (list (parse-pin row bytes-marker 'bytes)
                     (parse-pin row string-marker 'string)))
  (define sizes (sort (remove-duplicates (map ceiling-size ceilings)) <))
  (define samples
    (for*/list ([p (in-list pins)]
                [run (in-range 1 (add1 runs))]
                [s (in-list (run/announce p sizes run runs))])
      s))
  (define cells (map (λ (c) (cell-for c samples runs)) ceilings))
  (report-header)
  (for ([c (in-list cells)]) (report-cell c))
  (displayln (verdict cells runs))
  (when (ormap (λ (c) (not (cell-ok? c))) cells) (exit 1)))

(module+ main
  ;; Command-line arguments arrive as a vector of byte strings, so decode them here and let
  ;; `main` take a list of ordinary strings.
  (main (vector->list (current-command-line-arguments))))
