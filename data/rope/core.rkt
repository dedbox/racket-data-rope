#lang racket/base

;;; data/rope/core.rkt -- the erased rope core.
;;;
;;; This module is the runtime heart of the library. It defines the rope data structure and
;;; the algorithms that manipulate it, and nothing else. It is deliberately free of the rest
;;; of the system: no macros, no rope "types", no type-class machinery, no hashing, no
;;; content equality. A maintainer who understands this file understands the whole data
;;; structure; everything built on top (types, operations, classes) is thin code that sits
;;; over these functions.
;;;
;;; Three facts are enough to read the file:
;;;
;;;   1. A rope is one of two sealed record types: `leaf` (a single chunk of data) or `node`
;;;      (two subtrees). Ropes are persistent and immutable -- every operation below returns
;;;      a new rope and leaves its arguments untouched.
;;;
;;;   2. The tree is leaf-oriented and height-balanced (AVL). Chunks live only in leaves; a
;;;      node merely caches its children's measures (length, width, height). Sibling heights
;;;      differ by at most one. Concatenation, splitting and slicing are all built on
;;;      `rope-join`, after Blelloch, Ferizovic and Sun, "Just Join for Parallel Ordered
;;;      Sets" (2016), which yields worst-case O(log n) with a single balance routine and no
;;;      rebuilds, thresholds or amortization.
;;;
;;;   3. The only type-specific behaviour is the chunk algebra: the primitive operations
;;;      that must look inside a chunk. It is implemented by the `chunk-dict` -- a
;;;      dictionary of those operations (length, ref, slice, append, ...), passed
;;;      explicitly as the first argument to exactly those functions that need it.
;;;      Everything else -- balance, navigation, iteration, structural sharing -- is
;;;      identical for every type and compiled once.
;;;
;;; In the project's design (see M0-notes.org) this is the "core" layer. The type layer
;;; (`define-rope-type`) turns a chunk-dict into a runtime constant plus a compile-time index;
;;; the operations and classes layers sit on top. Nothing here depends on either, so this
;;; file can be compiled, tested and benchmarked on its own.
;;;
;;; Editing notes:
;;;   * `σ` is the conventional name for a chunk-dict argument; `ρ` (rho) for a rope.
;;;   * Both structs are `#:authentic #:sealed`, so every predicate and accessor is a single
;;;     record-type test. That is the whole point of erasure: one pair of structs for every
;;;     type, no per-type subtypes, no per-node closures, no runtime type tag.
;;;   * `empty-rope` is the single empty rope for every type. No operation ever builds an
;;;     empty leaf; they all return `empty-rope` instead. Keep it that way.
;;;   * `rope-valid?` checks every structural invariant. Run it in tests after every random
;;;     operation; if balance or size invariants fail there, the bug is in the join/split.

(provide (all-defined-out))

;; ---------------------------------------------------------------------------------------
;; Data
;; ---------------------------------------------------------------------------------------
;;
;; A rope is one of two authentic, sealed record types. Sealing makes every predicate and
;; accessor a single record-type test (no structural comparison), which matters because the
;; whole point of this layer is erasure: the same two structs serve every rope type.
;;
;;   length : number of elements in the rope
;;   width  : number of valid indices spanning those elements (the additive measure; equal to
;;            `length` for element types whose elements all occupy one index, e.g. characters)
;;   chunk  : the underlying data of a leaf (a string, bytes, vector, ...)
(struct leaf (length width chunk) #:authentic #:sealed)
;;
;;   height : tree height (0 for a leaf); siblings differ in height by at most one
;;   left   : left subtree
;;   right  : right subtree
(struct node (length width height left right) #:authentic #:sealed)

;; The single empty rope, shared by every type. It is a leaf of length 0 with no chunk.
;; Every operation returns this instead of constructing an empty leaf, so structural sharing
;; of empties is free and `eq?` tests against it are meaningful.
(define empty-rope (leaf 0 0 #f))

;; ---------------------------------------------------------------------------------------
;; The chunk algebra, implemented as a chunk dictionary
;; ---------------------------------------------------------------------------------------
;;
;; The chunk algebra is the concept: the operations that must look inside a chunk. This
;; sealed record is its implementation -- the chunk dictionary -- and the ONLY
;; type-specific part of the whole system. A rope carries no reference to it: it is passed
;; explicitly to the few functions that need it, which is what keeps ropes uniform and
;; erased. Defining a new rope type means supplying one of these dictionaries and passing
;; it to the functions below; the tree code itself never changes.
;;
;;   empty       : the empty chunk
;;   length      : chunk → nat        (element count of a chunk)
;;   ref         : chunk nat → element
;;   slice       : chunk nat nat → chunk   (Racket's convention: [start, end))
;;   append      : chunk chunk → chunk
;;   width       : chunk → nat        (additive measure of a chunk)
;;   limit       : the largest chunk a leaf may hold
;;   merge-limit : boundary leaves are merged on concat when their combined length is <= this
(struct chunk-dict (empty length ref slice append width limit merge-limit)
  #:authentic #:sealed)

;; Constructor for chunk-dict. `width` may be either a procedure on chunks or a constant
;; width per element; a constant is wrapped so that `width` always behaves as a procedure.
(define (make-chunk-dict #:empty e #:length len #:ref ref #:slice slice #:append app
                        #:width [width 1] #:limit limit #:merge-limit [merge-limit limit])
  (chunk-dict e len ref slice app
             (if (procedure? width) width (λ (c) (* width (len c))))
             limit merge-limit))

;; ---------------------------------------------------------------------------------------
;; Measures shared by leaves and nodes
;; ---------------------------------------------------------------------------------------
;;
;; These four accessors hide the leaf/node distinction so the rest of the code can reason
;; about "a rope" without pattern matching. `rope-height` is 0 for a leaf (a single leaf is
;; a tree of height 0) and `node-height` for a node.
(define (rope? x) (or (leaf? x) (node? x)))
(define (rope-length ρ) (if (leaf? ρ) (leaf-length ρ) (node-length ρ)))
(define (rope-width  ρ) (if (leaf? ρ) (leaf-width  ρ) (node-width  ρ)))
(define (rope-height ρ) (if (leaf? ρ) 0 (node-height ρ)))
(define (rope-empty? ρ) (= (rope-length ρ) 0))

;; Unpack a node's two children as values. Kept as a single function so that, should the
;; node representation ever change, the destructure lives in one place.
(define (node-children ρ)
  (values (node-left ρ) (node-right ρ)))

;; ---------------------------------------------------------------------------------------
;; Constructors
;; ---------------------------------------------------------------------------------------

;; Builds a node from two already-balanced ropes, recomputing the cached measures. The
;; caller (rope-join) is responsible for only calling this when the result stays balanced.
(define (make-node l r)
  (node (+ (rope-length l) (rope-length r))
        (+ (rope-width l) (rope-width r))
        (add1 (max (rope-height l) (rope-height r)))
        l r))

;; Builds a leaf from a chunk, consulting the chunk-dict for its measures. A zero-length
;; chunk collapses to the shared empty rope rather than producing an empty leaf.
(define (make-leaf σ c)
  (define n ((chunk-dict-length σ) c))
  (if (= n 0) empty-rope (leaf n ((chunk-dict-width σ) c) c)))

;; ---------------------------------------------------------------------------------------
;; Building a tree from a single chunk
;; ---------------------------------------------------------------------------------------
;;
;; Splits an arbitrarily long chunk into leaves of at most `limit` elements and arranges
;; them as a balanced tree. The split point is chosen so that sibling leaf counts differ by
;; at most one, which makes the result AVL-balanced by construction. The middle chunk is
;; reused verbatim (no copy) when it spans the whole input; otherwise it is sliced.
(define (chunk->rope σ c)
  (define n ((chunk-dict-length σ) c))
  (define limit (chunk-dict-limit σ))
  (define slice (chunk-dict-slice σ))
  (let build ([i 0] [j n])
    (define Δ (- j i))
    (cond [(= Δ 0) empty-rope]
          [(<= Δ limit) (make-leaf σ (if (and (= i 0) (= j n)) c (slice c i j)))]
          [else
           (define P (quotient (+ Δ (sub1 limit)) limit)) ; ⌈Δ/limit⌉ (span cardinality)
           (define k (+ i (* (quotient P 2) limit)))      ; i + ⌊P/2⌋·limit (split point)
           (make-node (build i k) (build k j))])))

;; ---------------------------------------------------------------------------------------
;; Tree rotation
;; ---------------------------------------------------------------------------------------
;;
;; Standard AVL rotations. Each takes a subtree `ρ` and returns a new subtree with the same
;; in-order sequence of leaves but a different shape; they are the repair step of `join`.
;; The ASCII diagrams below show the before (left) and after (right) of each rotation.

;;   ρ                 r′
;;  / \               / \
;; l   r     ==>    ρ′   rr
;;    / \          / \
;;  rl   rr       l   rl
(define (rotate-left ρ)
  (define-values (l r) (node-children ρ))
  (define-values (rl rr) (node-children r))
  (make-node (make-node l rl) rr))

;;      ρ            l′
;;     / \          / \
;;    l   r  ==>  ll   ρ′
;;   / \              / \
;; ll   lr          lr   r
(define (rotate-right ρ)
  (define-values (l r) (node-children ρ))
  (define-values (ll lr) (node-children l))
  (make-node ll (make-node lr r)))

;; ---------------------------------------------------------------------------------------
;; Asymmetric tree join
;; ---------------------------------------------------------------------------------------
;;
;; `rope-join` concatenates two non-empty balanced ropes into one balanced rope. It is the
;; single routine from which concat, split and slice are built, so it is the most important
;; function in the file and the only place balance is actually decided.
;;
;; The algorithm is the "asymmetric join" of Blelloch, Ferizovic and Sun (2016). If the two
;; trees are already within one height of each other, a single node suffices. Otherwise we
;; descend the inner spine of the taller tree (the right spine of `l` in `join-right`, the
;; left spine of `r` in `join-left`) until the heights are within one, merge there, and
;; repair balance on the way back up with at most one single or one double rotation per
;; level. This gives worst-case O(log n) with a single balance routine and no rebuilds.
;;
;; Why it applies to this leaf-oriented tree: Blelloch et al. describe join for key-bearing
;; nodes; a leaf-oriented tree with L leaves has the same shape as a key tree with L-1 nodes
;; whose empty subtrees are the leaves. Joining without a middle key is joining with a dummy
;; key, so the algorithm carries over unchanged. The invariant tests in the test suite check
;; balance after every random operation, precisely because this derivation is by hand.

;; Moves down the right sub-tree of l until a node whose height is ≤ height(r) is found,
;; then merges r.
;;
;; Precondition: height(l) > height(r) + 1 ⇒ l is a node
(define (join-right l r)
  (define-values (ll lr) (node-children l))
  (cond [(<= (rope-height lr) (add1 (rope-height r)))
         (define ρ (make-node lr r))
         (if (<= (rope-height ρ) (add1 (rope-height ll)))
             ;;    ·
             ;;   / \
             ;; ll   ρ
             ;;     / \
             ;;   lr   r
             (make-node ll ρ)
             ;;        ρ            lr′             ·₀                     _ lr′′_
             ;;       / \          /  \            /  \                   /       \
             ;;     lr   r  ==>  ·₁    ρ′    ↝    ll    lr′     ==>      ·₀′        ρ′
             ;;    /  \               / \              /  \             /  \       / \
             ;;  ·₁    ·₂           ·₂   r           ·₁    ρ′         ll    ·₁   ·₂   r
             ;;                                           / \
             ;;                                         ·₂   r
             (rotate-left (make-node ll (rotate-right ρ))))]
        [else
         (define ρ (join-right lr r))
         (define ρ* (make-node ll ρ))
         (if (<= (rope-height ρ) (add1 (rope-height ll)))
             ;;    ρ*
             ;;   /  \
             ;; ll    ρ  
             ;;      / \
             ;;    lr   r
             ρ*
             ;;    ρ*                  ρ′
             ;;   /  \                / \
             ;; ll    ρ     ==>     ρ*′  r
             ;;      / \           /  \
             ;;    lr   r        ll   lr
             (rotate-left ρ*))]))

;; Precondition: height(r) > height(l) + 1 ⇒ r is a node
(define (join-left l r)
  (define-values (rl rr) (node-children r))
  (cond [(<= (rope-height rl) (add1 (rope-height l)))
         (define ρ (make-node l rl))
         (if (<= (rope-height ρ) (add1 (rope-height rr)))
             ;;     ·
             ;;    / \
             ;;   ρ   rr
             ;;  / \
             ;; l   rl
             (make-node ρ rr)
             ;;   ρ                 rl′                _ ·₀ _              _ rl′′ _
             ;;  / \               /  \               /      \            /        \
             ;; l   rl     ==>    ρ′   ·₂    ↝      rl′       rr  ==>    ρ′         ·₀′
             ;;    /  \          / \               /  \                 / \        /  \
             ;;  ·₁    ·₂       l   ·₁            ρ′   ·₂              l   ·₁    ·₂    rr
             ;;                                  / \
             ;;                                 l   ·₁
             (rotate-right (make-node (rotate-left ρ) rr)))]
        [else
         (define ρ (join-left l rl))
         (define ρ* (make-node ρ rr))
         (if (<= (rope-height ρ) (add1 (rope-height rr)))
             ;;     ρ*
             ;;    /  \
             ;;   ρ    rr
             ;;  / \
             ;; l   rl
             ρ*
             ;;     ρ*            ρ′
             ;;    /  \          / \
             ;;   ρ    rr  ==>  l   ρ*′
             ;;  / \               /  \
             ;; l   rl           rl    rr
             (rotate-right ρ*))]))

;; The public entry point. Dispatches on the height difference and falls back to a plain
;; node when the two trees are already balanced against each other.
(define (rope-join l r)
  (define hl (rope-height l))
  (define hr (rope-height r))
  (cond [(> hl (add1 hr)) (join-right l r)]
        [(> hr (add1 hl)) (join-left l r)]
        [else (make-node l r)]))

;; ---------------------------------------------------------------------------------------
;; Boundary-leaf helpers
;; ---------------------------------------------------------------------------------------
;;
;; These locate and rewrite the outermost leaves, which is where concat needs to look in
;; order to merge boundary chunks. `replace-last-leaf` preserves the tree shape (so no
;; rebalancing is needed); `drop-first-leaf` removes a leaf and rebalances the remainder.

(define (first-leaf ρ) (if (leaf? ρ) ρ (first-leaf (node-left ρ))))
(define (last-leaf ρ) (if (leaf? ρ) ρ (last-leaf (node-right ρ))))

;; Replaces the last leaf of `ρ` by `r` without changing the tree shape, so the result stays
;; balanced. Used by concat to fold the merged boundary chunk back into the left rope.
(define (replace-last-leaf ρ r)
  (if (leaf? ρ) r (make-node (node-left ρ) (replace-last-leaf (node-right ρ) r))))

;; Removes the first leaf of `ρ`. Returns the shared empty rope when `ρ` is itself a leaf;
;; otherwise rebalances the two halves with `rope-join`.
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
         (cond [(<= (+ (leaf-length a) (leaf-length b)) (chunk-dict-merge-limit σ))
                (define a+b (make-leaf σ ((chunk-dict-append σ) (leaf-chunk a) (leaf-chunk b))))
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
           (values (make-leaf σ ((chunk-dict-slice σ) c 0 i))
                   (make-leaf σ ((chunk-dict-slice σ) c i (leaf-length ρ))))]
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
          [(leaf? ρ) (make-leaf σ ((chunk-dict-slice σ) (leaf-chunk ρ) i j))]
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
;; reference is the only place the chunk-dict is consulted on this path, so on a leaf the
;; element is read directly. (See M0-notes.org: the hot-path optimization inlines this leaf case
;; and shares the tree walk.)
(define (rope-ref σ ρ i)
  (let loop ([ρ ρ] [i i])
    (if (leaf? ρ)
        ((chunk-dict-ref σ) (leaf-chunk ρ) i)
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
;; the real invariant checker: it is what proves the tree is still balanced and that every
;; cached measure is correct after an operation. Run it in tests after every random
;; operation; a failure there means a bug in join, split, slice or concat.

(define (rope-leaf-count ρ)
  (if (leaf? ρ)
      (if (= (leaf-length ρ) 0) 0 1)
      (+ (rope-leaf-count (node-left ρ)) (rope-leaf-count (node-right ρ)))))

(define (rope-node-count ρ)
  (if (leaf? ρ) 0
      (add1 (+ (rope-node-count (node-left ρ)) (rope-node-count (node-right ρ))))))

;; #t when every structural invariant holds: cached length/width/height are correct, sibling
;; heights differ by at most one, non-empty leaves hold at most `limit` elements, and the
;; only zero-length rope is the shared empty rope at the top. `top?` distinguishes the root
;; (which may legitimately be `empty-rope`) from an empty leaf elsewhere (which is an error).
(define (rope-valid? σ ρ)
  (define limit (chunk-dict-limit σ))
  (let check ([ρ ρ] [top? #t])
    (cond [(leaf? ρ) (if (= (leaf-length ρ) 0)
                         (and top? (eq? ρ empty-rope))
                         (and (<= (leaf-length ρ) limit)
                              (= (leaf-length ρ) ((chunk-dict-length σ) (leaf-chunk ρ)))
                              (= (leaf-width ρ) ((chunk-dict-width σ) (leaf-chunk ρ)))))]
          [else
           (define-values (l r) (node-children ρ))
           (and (check l #f)
                (check r #f)
                (= (node-length ρ) (+ (rope-length l) (rope-length r)))
                (= (node-width ρ) (+ (rope-width l) (rope-width r)))
                (= (node-height ρ) (add1 (max (rope-height l) (rope-height r))))
                (<= (abs (- (rope-height l) (rope-height r))) 1))])))
