#lang racket/base

;;; data/rope/treap-core.rkt -- the erased rope core built on join-based treaps.
;;;
;;; This module is the same algorithmic layer as data/rope/core.rkt, with the height-balanced
;;; (AVL) tree replaced by a randomized binary search tree (a treap). It is deliberately
;;; self-contained: it redefines `leaf`, `node`, `chunk-sig` and the whole API, so a call site
;;; can swap one file for the other by changing a single `require`. That is how the two tree
;;; families are compared (see notes.org §5 and benchmarks/treap-bench.rkt).
;;;
;;; Three facts are enough to read the file:
;;;
;;;   1. A rope is one of two sealed record types: `leaf` (a single chunk of data) or `node`
;;;      (two subtrees). Ropes are persistent and immutable -- every operation below returns
;;;      a new rope and leaves its arguments untouched.
;;;
;;;   2. The tree is leaf-oriented and *probabilistically* balanced. Chunks live only in
;;;      leaves; a node caches its children's measures (length, width) and the random priority
;;;      of the boundary between them. Balance comes from priorities alone: each node's
;;;      priority is greater than or equal to its descendants', so the tree is the Cartesian
;;;      tree of its boundary sequence. Expected depth is O(log n); there is no worst-case
;;;      guarantee, and no height is stored.
;;;
;;;   3. The only type-specific behaviour is the `chunk-sig` (the "chunk algebra" or
;;;      dictionary): a sealed record of the primitive operations on a chunk (length, ref,
;;;      slice, append, ...). It is passed explicitly as the first argument to exactly those
;;;      functions that must look inside a chunk. Everything else -- balance, navigation,
;;;      iteration, structural sharing -- is identical for every type and compiled once.
;;;
;;; Why a treap rather than AVL:
;;;   * One comparison per level instead of two height comparisons plus a repair branch, and no
;;;     height field to recompute on every node copy.
;;;   * `rope-join` needs no rotation cases: it descends one spine and writes one node there.
;;;     That makes the join's code path a single spine walk, which is the hot path for concat,
;;;     split, slice and splice.
;;;   * `chunk->rope` builds a *perfectly* balanced tree of shape (priorities are drawn under
;;;     the parent's bound), so iteration and structural sharing are cache-friendly even though
;;;     the shape is randomized by later joins.
;;; The cost is that balance is probabilistic: a treap's depth is O(log n) in expectation and up
;;; to O(n) in the worst case, with probability ~n/2^31 of a given level being skipped. Treap
;;; join/split cost is stated as expected O(log n), so the API carries the same asymptotic
;;; bounds but not the worst-case ones. See benchmarks/treap-bench.rkt for the measured
;;; comparison, and notes.org §5.2 for the benchmark plan those numbers feed.
;;;
;;; Editing notes:
;;;   * `σ` is the conventional name for a chunk-sig argument; `ρ` (rho) for a rope.
;;;   * Both structs are `#:authentic #:sealed`, so every predicate and accessor is a single
;;;     record-type test. That is the whole point of erasure: one pair of structs for every
;;;     type, no per-type subtypes, no per-node closures, no runtime type tag.
;;;   * `empty-rope` is the single empty rope for every type. No operation ever builds an
;;;     empty leaf; they all return `empty-rope` instead. Keep it that way.
;;;   * `rope-valid?` checks every structural invariant. Run it in tests after every random
;;;     operation; if the heap-order or measure invariants fail there, the bug is in the join.
;;;   * Priorities come from Racket's own `random`, so `random-seed` makes every randomized
;;;     test reproducible. Ranges are 31 bits, so a collision is possible on a rope with
;;;     hundreds of thousands of boundaries; ties are resolved in favour of the earlier
;;;     boundary, which is what the invariant checker accepts as well (parent ≥ children).

(provide (all-defined-out))

;; ---------------------------------------------------------------------------------------
;; Data
;; ---------------------------------------------------------------------------------------
;;
;; A rope is one of two authentic, sealed record types. Sealing makes every predicate and
;; accessor a single record-type test (no structural comparison), which matters because the
;; whole point of this layer is erasure: the same two structs serve every rope type.
;;
;;   length   : number of elements in the rope
;;   width    : number of valid indices spanning those elements (the additive measure; equal to
;;              `length` for element types whose elements all occupy one index, e.g. characters)
;;   chunk    : the underlying data of a leaf (a string, bytes, vector, ...)
(struct leaf (length width chunk) #:authentic #:sealed)
;;
;;   priority : the boundary priority of this node; ≥ that of both children (the heap-order
;;              invariant), and the whole balance mechanism. Leaves carry no priority: they are
;;              the "nil" of the key-oriented formulation, so `rope-priority` returns ⊥ for them.
;;   left     : left subtree
;;   right    : right subtree
(struct node (length width priority left right) #:authentic #:sealed)

;; The single empty rope, shared by every type. It is a leaf of length 0 with no chunk.
;; Every operation returns this instead of constructing an empty leaf, so structural sharing
;; of empties is free and `eq?` tests against it are meaningful.
(define empty-rope (leaf 0 0 #f))

;; ---------------------------------------------------------------------------------------
;; The chunk algebra (a.k.a. the chunk dictionary)
;; ---------------------------------------------------------------------------------------
;;
;; This sealed record is the ONLY type-specific part of the whole system. It bundles the
;; primitive operations that must look inside a chunk. A rope carries no reference to it --
;; it is passed explicitly to the few functions that need it -- which is what keeps ropes
;; uniform and erased. Defining a new rope type (see notes.org) means defining one of these
;; and passing it to the functions below; the tree code itself never changes.
;;
;;   empty       : the empty chunk
;;   length      : chunk → nat        (element count of a chunk)
;;   ref         : chunk nat → element
;;   slice       : chunk nat nat → chunk   (Racket's convention: [start, end))
;;   append      : chunk chunk → chunk
;;   width       : chunk → nat        (additive measure of a chunk)
;;   limit       : the largest chunk a leaf may hold
;;   merge-limit : boundary leaves are merged on concat when their combined length is <= this
(struct chunk-sig (empty length ref slice append width limit merge-limit)
  #:authentic #:sealed)

;; Constructor for chunk-sig. `width` may be either a procedure on chunks or a constant
;; width per element; a constant is wrapped so that `width` always behaves as a procedure.
(define (make-chunk-sig #:empty e #:length len #:ref ref #:slice slice #:append app
                        #:width [width 1] #:limit limit #:merge-limit [merge-limit limit])
  (chunk-sig e len ref slice app
             (if (procedure? width) width (λ (c) (* width (len c))))
             limit merge-limit))

;; ---------------------------------------------------------------------------------------
;; Priorities
;; ---------------------------------------------------------------------------------------
;;
;; A boundary priority is a non-negative fixnum drawn from [0, priority-top). The top value is
;; 2^31, which is inside Racket's `random` integer range and a fixnum comparison, so a priority
;; test is one fixnum comparison per level -- the same cost AVL pays for its two height tests,
;; and cheaper than recomputing a height.
(define priority-top (expt 2 31))

;; The priority of a leaf (a nil subtree): lower than every real priority, so a leaf is never
;; the pivot of a join.
(define ⊥ -1)

;; A priority in the full range. `rope-join` draws one of these per call: it is the priority of
;; the boundary the join inserts between the two ropes.
(define (fresh-priority) (random priority-top))

;; A priority strictly below `bound`, used by `chunk->rope` to keep a constructed tree's
;; priorities decreasing along depth (the heap-order invariant) while still randomizing them.
(define (bounded-priority [bound priority-top])
  (if (< bound 2) 0 (random bound)))

;; ---------------------------------------------------------------------------------------
;; Measures shared by leaves and nodes
;; ---------------------------------------------------------------------------------------
;;
;; These accessors hide the leaf/node distinction so the rest of the code can reason about
;; "a rope" without pattern matching. Unlike the AVL core, height is not a cached measure: a
;; treap's shape is determined by priorities, so `rope-height` walks the whole tree and is a
;; diagnostic, not part of the algorithm. `rope-priority` is the only stored balance measure.
(define (rope? x) (or (leaf? x) (node? x)))
(define (rope-length ρ) (if (leaf? ρ) (leaf-length ρ) (node-length ρ)))
(define (rope-width  ρ) (if (leaf? ρ) (leaf-width  ρ) (node-width  ρ)))
(define (rope-priority ρ) (if (leaf? ρ) ⊥ (node-priority ρ)))
(define (rope-empty? ρ) (= (rope-length ρ) 0))

;; The tree height (number of node levels on the deepest path), computed in O(n). A treap's
;; height is expected O(log n) but not an invariant, so this exists for diagnostics, the
;; balance table in benchmarks/treap-bench.rkt, and the depth bound a test asserts.
(define (rope-height ρ)
  (if (leaf? ρ)
      0
      (add1 (max (rope-height (node-left ρ)) (rope-height (node-right ρ))))))

;; Unpack a node's two children as values. Kept as a single function so that, should the
;; node representation ever change, the destructure lives in one place.
(define (node-children ρ)
  (values (node-left ρ) (node-right ρ)))

;; ---------------------------------------------------------------------------------------
;; Constructors
;; ---------------------------------------------------------------------------------------

;; Builds a node from its boundary priority and two subtrees, recomputing the cached measures.
;; Every structural change in this file goes through this function: the priority is the repair
;; that AVL's rotations would otherwise provide, so `make-node` must be given the priority the
;; caller drew for the boundary it is creating.
(define (make-node/priority priority l r)
  (node (+ (rope-length l) (rope-length r))
        (+ (rope-width l) (rope-width r))
        priority
        l r))

;; Builds a node with a fresh boundary priority. The convenience form for callers that create a
;; junction without descending (test fixtures, and hand-built trees from library code).
(define (make-node l r)
  (make-node/priority (fresh-priority) l r))

;; Builds a leaf from a chunk, consulting the chunk-sig for its measures. A zero-length
;; chunk collapses to the shared empty rope rather than producing an empty leaf.
(define (make-leaf σ c)
  (define n ((chunk-sig-length σ) c))
  (if (= n 0) empty-rope (leaf n ((chunk-sig-width σ) c) c)))

;; ---------------------------------------------------------------------------------------
;; Building a tree from a single chunk
;; ---------------------------------------------------------------------------------------
;;
;; Splits an arbitrarily long chunk into leaves of at most `limit` elements and arranges them
;; as a *perfectly* balanced tree: the split point is chosen so that sibling leaf counts differ
;; by at most one, so the shape is as shallow as the leaf count allows. Priorities descend
;; strictly under their parent's bound, which makes the result a treap whose root is the
;; highest boundary, while leaving the priorities themselves random -- later joins draw from the
;; full range and so behave exactly as if this tree were built by joins.
;;
;; The middle chunk is reused verbatim (no copy) when it spans the whole input; otherwise it is
;; sliced.
(define (chunk->rope σ c)
  (define n ((chunk-sig-length σ) c))
  (define limit (chunk-sig-limit σ))
  (define slice (chunk-sig-slice σ))
  (let build ([i 0] [j n] [bound priority-top])
    (define Δ (- j i))
    (cond [(= Δ 0) empty-rope]
          [(<= Δ limit) (make-leaf σ (if (and (= i 0) (= j n)) c (slice c i j)))]
          [else
           (define P (quotient (+ Δ (sub1 limit)) limit)) ; ⌈Δ/limit⌉ (span cardinality)
           (define k (+ i (* (quotient P 2) limit)))      ; i + ⌊P/2⌋·limit (split point)
           (define p (bounded-priority bound))
           (make-node/priority p (build i k p) (build k j p))])))

;; ---------------------------------------------------------------------------------------
;; Asymmetric tree join
;; ---------------------------------------------------------------------------------------
;;
;; `rope-join` concatenates two ropes into one. It is the single routine from which concat,
;; split and slice are built, so it is the most important function in the file and the only
;; place the structure is decided.
;;
;; The algorithm is a treap `join`: the boundary joining the two ropes gets a fresh priority p,
;; and the result is the Cartesian tree of the joined boundary sequence. At each step the node
;; with the larger priority survives as an ancestor and the other subtree descends one level;
;; the walk stops as soon as p exceeds both subtree priorities, where the junction node is
;; written. There is no rotation and no height comparison: the priority comparison IS the repair
;; step. Cost: one priority comparison and one node allocation per level, expected O(log n)
;; levels.
;;
;; Mapping to the leaf-oriented tree: a leaf-oriented tree with L leaves is a key-oriented tree
;; with L−1 nodes whose nil subtrees are the leaves, so a node's priority is the priority of the
;; dummy key that separates its children, and a leaf has no priority (`⊥`, lower than any
;; boundary). Joining without a middle element is joining with a dummy key whose priority is
;; drawn fresh (see notes.org §3.2 for the same derivation as the AVL core).

;; The join, stated as a walk down one spine of whichever subtree wins the priority comparison.
;; `p` is the priority of the boundary being inserted, which lies between `l` and `r`.
;;
;; Precondition: neither operand is empty. When the branch descends, the operand being descended
;; into is a node: ⊥ is the smallest priority, so a leaf operand is always beaten by p.
(define (rope-join l r)
  (cond [(eq? l empty-rope) r]
        [(eq? r empty-rope) l]
        [else (let join ([l l] [r r] [p (fresh-priority)])
                (define pl (rope-priority l))
                (define pr (rope-priority r))
                (cond [(> p (max pl pr))
                       ;; The junction boundary wins: it becomes the node under both operands.
                       ;;    ·
                       ;;   / \
                       ;;  l   r
                       (make-node/priority p l r)]
                      [(>= pl pr)
                       ;; The left boundary survives as an ancestor, and the junction keeps
                       ;; descending down l's right spine. Ties go to the earlier boundary.
                       ;;   l′
                       ;;  / \
                       ;; ll  join(lr, r)
                       ;;    / \
                       ;;   lr  r
                       (make-node/priority pl (node-left l) (join (node-right l) r p))]
                      [else
                       ;; Mirror of the above: descend r's left spine.
                       ;;      r′
                       ;;     / \
                       ;; join(l, rl)  rr
                       ;;     / \
                       ;;    l   rl
                       (make-node/priority pr (join l (node-left r) p) (node-right r))]))]))

;; ---------------------------------------------------------------------------------------
;; Boundary-leaf helpers
;; ---------------------------------------------------------------------------------------
;;
;; These locate and rewrite the outermost leaves, which is where concat needs to look in
;; order to merge boundary chunks. `replace-last-leaf` preserves the tree shape (so no repair
;; is needed -- a treap's shape is its priorities, and replacing a leaf changes no priority);
;; `drop-first-leaf` removes a leaf and rebuilds the remainder with `rope-join`.

(define (first-leaf ρ) (if (leaf? ρ) ρ (first-leaf (node-left ρ))))
(define (last-leaf ρ) (if (leaf? ρ) ρ (last-leaf (node-right ρ))))

;; Replaces the last leaf of `ρ` by `r` without changing the tree shape. Used by concat to fold
;; the merged boundary chunk back into the left rope.
(define (replace-last-leaf ρ r)
  (if (leaf? ρ) r (make-node/priority (node-priority ρ) (node-left ρ) (replace-last-leaf (node-right ρ) r))))

;; Removes the first leaf of `ρ`. Returns the shared empty rope when `ρ` is itself a leaf;
;; otherwise rebuilds the two halves with `rope-join`.
(define (drop-first-leaf ρ)
  (cond [(leaf? ρ) empty-rope]
        [else
         (define l (drop-first-leaf (node-left ρ)))
         (if (eq? l empty-rope) (node-right ρ) (rope-join l (node-right ρ)))]))

;; ---------------------------------------------------------------------------------------
;; Concatenation
;; ---------------------------------------------------------------------------------------

;; Concatenates two ropes. When the last leaf of the left rope and the first leaf of the
;; right rope are small enough to merge (combined length ≤ `merge-limit`), they are spliced
;; into a single leaf by one path copy; otherwise the two trees are simply joined. This keeps
;; boundary fragmentation low for repeated appends and splices.
(define (rope-concat σ l r)
  (cond [(rope-empty? l) r]
        [(rope-empty? r) l]
        [else
         (define a (last-leaf l))
         (define b (first-leaf r))
         (cond [(<= (+ (leaf-length a) (leaf-length b)) (chunk-sig-merge-limit σ))
                (define a+b (make-leaf σ ((chunk-sig-append σ) (leaf-chunk a) (leaf-chunk b))))
                (define l′ (replace-last-leaf l a+b))
                (define r′ (drop-first-leaf r))
                (if (rope-empty? r′) l′ (rope-join l′ r′))]
               [else (rope-join l r)])]))

;; ---------------------------------------------------------------------------------------
;; Splitting and slicing
;; ---------------------------------------------------------------------------------------
;;
;; `rope-split` divides a rope at an index; `rope-slice` returns a sub-range; `rope-splice`
;; replaces a sub-range. All three are O(log n): they copy a path and rebuild, reusing the
;; untouched subtrees (structural sharing). Each descends by comparing the target index with
;; the cached left-subtree length, and slices the one chunk it touches.
;;
;; Returns (values prefix suffix), where |prefix| = i.
(define (rope-split σ ρ i)
  (let split ([ρ ρ] [i i])
    (cond [(= i 0) (values empty-rope ρ)]
          [(= i (rope-length ρ)) (values ρ empty-rope)]
          [(leaf? ρ)
           (define c (leaf-chunk ρ))
           (values (make-leaf σ ((chunk-sig-slice σ) c 0 i))
                   (make-leaf σ ((chunk-sig-slice σ) c i (leaf-length ρ))))]
          [else
           (define-values (l r) (node-children ρ))
           (define n (rope-length l))
           (cond [(< i n) (define-values (a b) (split l i)) (values a (rope-join b r))]
                 [(> i n) (define-values (a b) (split r (- i n))) (values (rope-join l a) b)]
                 [else (values l r)])])))

;; Returns the elements in [i, j). The full range returns the rope unchanged (structural
;; sharing); the empty range returns the shared empty rope.
(define (rope-slice σ ρ i j)
  (let slice ([ρ ρ] [i i] [j j])
    (cond [(= i j) empty-rope]
          [(and (= i 0) (= j (rope-length ρ))) ρ]
          [(leaf? ρ) (make-leaf σ ((chunk-sig-slice σ) (leaf-chunk ρ) i j))]
          [else
           (define-values (l r) (node-children ρ))
           (define n (rope-length l))
           (cond [(<= j n) (slice l i j)]
                 [(>= i n) (slice r (- i n) (- j n))]
                 [else (rope-join (slice l i n) (slice r 0 (- j n)))])])))

;; Replaces the elements in [i, j) with `ρ*`: split off the prefix and the to-be-dropped
;; middle, split off the suffix, and concatenate prefix ++ insertion ++ suffix.
(define (rope-splice σ ρ i j ρ*)
  (define-values (prefix rest) (rope-split σ ρ i))
  (define-values (_dropped suffix) (rope-split σ rest (- j i)))
  (rope-concat σ (rope-concat σ prefix ρ*) suffix))

;; ---------------------------------------------------------------------------------------
;; Access and iteration
;; ---------------------------------------------------------------------------------------

;; Returns the element at index `i`. Descends by the cached left-subtree length; the chunk
;; reference is the only place the chunk-sig is consulted on this path, so on a leaf the
;; element is read directly. (See notes.org: the hot-path optimization inlines this leaf case
;; and shares the tree walk.)
(define (rope-ref σ ρ i)
  (let loop ([ρ ρ] [i i])
    (if (leaf? ρ)
        ((chunk-sig-ref σ) (leaf-chunk ρ) i)
        (let ([n (rope-length (node-left ρ))])
          (if (< i n)
              (loop (node-left ρ) i)
              (loop (node-right ρ) (- i n)))))))

;; Folds over the chunks (not the elements) left-to-right: (proc acc chunk) -> acc. Empty
;; leaves contribute nothing. The right subtree is visited after the left by nesting the
;; left fold inside the right fold's accumulator, so no explicit stack is needed.
(define (rope-fold-chunks proc init ρ)
  (let loop ([ρ ρ] [acc init])
    (cond [(leaf? ρ) (if (= (leaf-length ρ) 0) acc (proc acc (leaf-chunk ρ)))]
          [else (loop (node-right ρ) (loop (node-left ρ) acc))])))

;; Returns the rope's chunks as a list, left-to-right.
(define (rope-chunks ρ)
  (reverse (rope-fold-chunks (λ (acc c) (cons c acc)) null ρ)))

;; ---------------------------------------------------------------------------------------
;; Structural statistics and validation
;; ---------------------------------------------------------------------------------------
;;
;; The two count functions are diagnostics (used by tests and benchmarks). `rope-valid?` is
;; the real invariant checker: it proves the tree is still a treap and that every cached
;; measure is correct after an operation. Run it in tests after every random operation; a
;; failure there means a bug in join, split, slice or concat.

(define (rope-leaf-count ρ)
  (if (leaf? ρ)
      (if (= (leaf-length ρ) 0) 0 1)
      (+ (rope-leaf-count (node-left ρ)) (rope-leaf-count (node-right ρ)))))

(define (rope-node-count ρ)
  (if (leaf? ρ) 0
      (add1 (+ (rope-node-count (node-left ρ)) (rope-node-count (node-right ρ))))))

;; #t when every structural invariant holds: cached length/width are correct, each node's
;; priority is ≥ that of both children (heap order: the tree is the Cartesian tree of its
;; boundary sequence), non-empty leaves hold at most `limit` elements, and the only zero-length
;; rope is the shared empty rope at the top. `top?` distinguishes the root (which may
;; legitimately be `empty-rope`) from an empty leaf elsewhere (which is an error).
;;
;; Priority ties are accepted as ≥, because 31-bit priorities can collide on a large rope and
;; `rope-join` resolves a tie in favour of the earlier boundary -- a rule no local comparison
;; can see. The in-order sequence is not checked here: `strof` and the model comparisons in the
;; test suite are what prove order.
(define (rope-valid? σ ρ)
  (define limit (chunk-sig-limit σ))
  (let check ([ρ ρ] [top? #t])
    (cond [(leaf? ρ) (if (= (leaf-length ρ) 0)
                         (and top? (eq? ρ empty-rope))
                         (and (<= (leaf-length ρ) limit)
                              (= (leaf-length ρ) ((chunk-sig-length σ) (leaf-chunk ρ)))
                              (= (leaf-width ρ) ((chunk-sig-width σ) (leaf-chunk ρ)))))]
          [else
           (define-values (l r) (node-children ρ))
           (define p (node-priority ρ))
           (and (check l #f)
                (check r #f)
                (= (node-length ρ) (+ (rope-length l) (rope-length r)))
                (= (node-width ρ) (+ (rope-width l) (rope-width r)))
                (>= p (rope-priority l))
                (>= p (rope-priority r))
                (>= p 0))])))
