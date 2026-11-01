#lang racket/base

;;; data/rope/core.rkt -- the type-erased rope core.
;;;
;;; This module defines the rope data structure and the algorithms that manipulate it. It
;;; is the runtime component of the library. Everything else builds on top of this file.
;;;
;;; === What a rope is ===
;;;
;;; A rope is a balanced binary tree whose leaves hold chunks of data (strings, bytes,
;;; vectors, etc.) and whose internal nodes cache subtree metadata. Ropes are persistent
;;; and immutable -- every operation returns a new rope, and no operation mutates its
;;; arguments.
;;;
;;; The tree is leaf-oriented and height-balanced (AVL). Concretely, chunks live only in
;;; the leaves, and siblings' heights differ by at most one. All core operations run in
;;; worst-case O(log n) time, per the asymmetric-join algorithm of Blelloch, Ferizovic &
;;; Sun (2016).
;;;
;;; Any type-specific behaviour is determined by a "chunk algebra" implemented as a set of
;;; chunk operations and passed explicitly to functions that need it. Balance, navigation,
;;; iteration, and node reuse behaviors are identical for every rope type.
;;;
;;; === Naming conventions ===
;;;
;;;   σ (sigma)  -- a chunk dictionary
;;;   ρ (rho)    -- a rope value
;;;
;;; === Key invariants ===
;;;
;;; - Both structs are `#:authentic #:sealed`. One pair of structs serves every rope type
;;;   (type erasure): no per-type subtypes, no per-node closures, no runtime tag. Sealing
;;;   makes every predicate and accessor a single record-type test.
;;;
;;; - `empty-rope` is the unique empty rope for every type. No operation ever builds an
;;;   empty leaf. Instead, they return this value, collapsing empty rope equality to pointer
;;;   equality (`eq?`).
;;;
;;; - `rope-valid?` checks every structural invariant, which currently includes cached
;;;    measures, AVL balance, and leaf-size caps. It should only be used for testing.

(provide (all-defined-out))

;; ---------------------------------------------------------------------------------------
;; Rope record types
;; ---------------------------------------------------------------------------------------

;; A leaf node holds a single chunk of data.
;;   length -- The number of elements in the chunk.
;;   width  -- The number of valid indices covered by an element. May differ for each
;;             element.
;;   chunk  -- The underlying data.
(struct leaf (length width chunk) #:authentic #:sealed)

;; An internal node caches metadata about its two subtrees. It never holds user data.
;;   height -- Tree height (0 for a leaf).
;;   left   -- The left subtree.
;;   right  -- The right subtree.
(struct node (length width height left right) #:authentic #:sealed)

;; The unique empty rope, shared across all types.
(define empty-rope (leaf 0 0 #f))

;; ---------------------------------------------------------------------------------------
;; The chunk algebra -- implemented as a chunk dictionary
;; ---------------------------------------------------------------------------------------
;;
;; A "chunk algebra" is the set of operations needed to interact with a chunk's contents,
;; implemented as a **chunk dictionary** struct type. This is the only type-specific part
;; of the entire system.
;;
;; A rope value contains no reference to it. Instead, callers pass it explicitly to
;; functions that need it, allowing each rope operation to have a uniform, type-erased
;; implementaton. Ropes are nominally typed -- each chunk-dict represents a distinct type.

;;   empty       -- the empty chunk value
;;   length      -- chunk → nat        (element count)
;;   ref         -- chunk nat → element
;;   slice       -- chunk nat nat → chunk   (Racket convention: [start, end))
;;   append      -- chunk chunk → chunk
;;   width       -- chunk → nat        (element index span; may be a procedure or a constant)
;;   limit       -- max elements per leaf
;;   merge-limit -- concat merges boundary leaves when combined length ≤ this
(struct chunk-dict (empty length ref slice append width limit merge-limit) #:authentic #:sealed)

;; smart constructor
(define (make-chunk-dict #:empty e #:length len #:ref ref #:slice slice #:append app
                         #:width [width 1] #:limit limit #:merge-limit [merge-limit limit])
  (chunk-dict e len ref slice app
              (if (procedure? width) width (λ (c) (* width (len c))))
              limit merge-limit))

;; ---------------------------------------------------------------------------------------
;; Measures -- uniform accessors over leaves and nodes
;; ---------------------------------------------------------------------------------------
;;
;; These hide the leaf/node distinction so callers can reason about "a rope" without
;; pattern matching. `rope-height` returns 0 for a leaf.

(define (rope? x) (or (leaf? x) (node? x)))
(define (rope-length ρ) (if (leaf? ρ) (leaf-length ρ) (node-length ρ)))
(define (rope-width  ρ) (if (leaf? ρ) (leaf-width  ρ) (node-width  ρ)))
(define (rope-height ρ) (if (leaf? ρ) 0 (node-height ρ)))
(define (rope-empty? ρ) (= (rope-length ρ) 0))

;; Unpack a node's children.
(define (node-children ρ) (values (node-left ρ) (node-right ρ)))

;; ---------------------------------------------------------------------------------------
;; Constructors
;; ---------------------------------------------------------------------------------------

;; Build a node from two already-balanced children, recomputing cached measures. It is the
;; caller's responsibility (e.g, `rope-join`) to guarantee that the result stays balanced.
(define (make-node l r)
  (node (+ (rope-length l) (rope-length r))
        (+ (rope-width l) (rope-width r))
        (add1 (max (rope-height l) (rope-height r)))
        l r))

;; Build a leaf from a chunk. A zero-length chunk collapses to the unique `empty-rope`
;; instead of a fresh empty leaf value.
(define (make-leaf σ c)
  (define n ((chunk-dict-length σ) c))
  (if (= n 0) empty-rope (leaf n ((chunk-dict-width σ) c) c)))

;; Splits an arbitrarily long chunk into leaves of at most `limit` elements and arranges
;; them as a balanced tree. The split point is chosen so that sibling leaf counts differ
;; by at most one, making the result AVL-balanced by construction (no rotations needed).
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
;; Tree rotation -- standard AVL rotations
;; ---------------------------------------------------------------------------------------
;;
;; Each rotation operation takes a subtree `ρ` and returns a new subtree which preserves
;; the order of elements but changes the internal shape.

;; Left rotation: the right child becomes the new root.
;;
;;     ρ                 r′
;;    / \               / \
;;   l   r     ==>    ρ′   rr
;;      / \          / \
;;    rl   rr       l   rl
(define (rotate-left ρ)
  (define-values (l r) (node-children ρ))
  (define-values (rl rr) (node-children r))
  (make-node (make-node l rl) rr))

;; Right rotation: the left child becomes the new root.
;;
;;       ρ            l′
;;      / \          / \
;;     l   r  ==>  ll   ρ′
;;    / \              / \
;;  ll   lr          lr   r
(define (rotate-right ρ)
  (define-values (l r) (node-children ρ))
  (define-values (ll lr) (node-children l))
  (make-node ll (make-node lr r)))

;; ---------------------------------------------------------------------------------------
;; Asymmetric tree join -- concatenation with rebalancing
;; ---------------------------------------------------------------------------------------
;;
;; `rope-join` concatenates two non-empty balanced ropes into one balanced rope. It is the
;; only place balance decisions are made, making it the most important function in this
;; file. All of the core rope operations (concat, split, slice) are built on it.
;;
;; The algorithm is the "asymmetric join" of Blelloch, Ferizovic & Sun (2016):
;;
;;   1. If the two trees are within one height of each other, a single fresh node suffices.
;;
;;   2. Otherwise descend the inner spine of the taller tree until heights are close, merge
;;      there, and repair balance on the way back up with at most one rotation per level.
;;
;; This requires worst-case O(log n) operations with a single balance routine and no
;; rebuilds.
;;
;; Blelloch et al. describe join for nodes carrying keys. A leaf-oriented tree with L
;; leaves has the same shape as a key tree with L−1 nodes whose empty subtrees are the
;; leaves. Joining without a middle key is the same as joining with a dummy key, so the
;; algorithm carries over unchanged.

;; Assuming l is taller than r, descend l's right spine until heights are close, then
;; merge r in and repair balance on the way back up.
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

;; Assuming r is taller than l, descend r's left spine until heights are close, then merge
;; l in and repair balance on the way back up.
;;
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

;; Dispatch on the height difference, falling back to a plain node when the two trees are
;; already balanced against each other.
(define (rope-join l r)
  (define hl (rope-height l))
  (define hr (rope-height r))
  (cond [(> hl (add1 hr)) (join-right l r)]
        [(> hr (add1 hl)) (join-left l r)]
        [else (make-node l r)]))

;; ---------------------------------------------------------------------------------------
;; Boundary-leaf helpers -- locate and rewrite outermost leaves
;; ---------------------------------------------------------------------------------------
;;
;; Concatenation needs to inspect the last leaf of the left rope and the first leaf of the
;; right rope. When both are small enough, they are merged into one chunk (boundary
;; merging) to keep fragmentation low for repeated appends. These helpers find and replace
;; those leaves without affecting the rest of the tree's shape.

(define (first-leaf ρ) (if (leaf? ρ) ρ (first-leaf (node-left ρ))))
(define (last-leaf ρ) (if (leaf? ρ) ρ (last-leaf (node-right ρ))))

;; Replace the last leaf of `ρ` with `r` without changing the tree's shape. Used by concat
;; to fold a merged boundary chunk back into the left rope.
(define (replace-last-leaf ρ r)
  (if (leaf? ρ) r (make-node (node-left ρ) (replace-last-leaf (node-right ρ) r))))

;; Remove the first leaf of `ρ`. Return the empty rope when `ρ` is a leaf, otherwise drop
;; the leaf and rebalance the two halves.
(define (drop-first-leaf ρ)
  (cond [(leaf? ρ) empty-rope]
        [else
         (define l (drop-first-leaf (node-left ρ)))
         (if (eq? l empty-rope) (node-right ρ) (rope-join l (node-right ρ)))]))

;; ---------------------------------------------------------------------------------------
;; Concatenation
;; ---------------------------------------------------------------------------------------

;; When the last leaf of `l` and the first leaf of `r` are small enough to merge (combined
;; length ≤ `merge-limit`), splice them into a single leaf in one path copy. Otherwise,
;; just join the two trees. This reduces fragmentation over repeated appends and splices.
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
;; Splitting and slicing -- O(log n) path-copy operations
;; ---------------------------------------------------------------------------------------
;;
;; These three functions rebuild one root-to-leaf path and reuse all unaffected subtrees,
;; requiring at most O(log n) updates. Each descends the tree by comparing the target
;; index against cached left-subtree lengths, and slices exactly one chunk along the path.

;; Return `(values prefix suffix)` where `|prefix| = i`.
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

;; Return the elements in [i, j). When `i = 0` and `j = |ρ|`, return `ρ` unchanged. When
;; `i = j`, return the empty rope.
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

;; Replace the elements in [i, j) with `ρ*`.
(define (rope-splice σ ρ i j ρ*)
  (define-values (prefix rest) (rope-split σ ρ i))
  (define-values (_dropped suffix) (rope-split σ rest (- j i)))
  (rope-concat σ (rope-concat σ prefix ρ*) suffix))

;; ---------------------------------------------------------------------------------------
;; Access and iteration
;; ---------------------------------------------------------------------------------------

;; Return the element at index `i`. Walk down the tree using cached left-subtree lengths.
;; On a leaf, read the element directly via the chunk-dict.
(define (rope-ref σ ρ i)
  (let loop ([ρ ρ] [i i])
    (if (leaf? ρ)
        ((chunk-dict-ref σ) (leaf-chunk ρ) i)
        (let ([n (rope-length (node-left ρ))])
          (if (< i n)
              (loop (node-left ρ) i)
              (loop (node-right ρ) (- i n)))))))

;; Fold over chunks (not elements) left-to-right: `(proc acc chunk) -> acc`. Empty leaves
;; contribute nothing. The right subtree is visited after the left by nesting the left fold
;; inside the right fold's accumulator, so no explicit stack is needed.
(define (rope-fold-chunks proc init ρ)
  (let loop ([ρ ρ] [acc init])
    (cond [(leaf? ρ) (if (= (leaf-length ρ) 0) acc (proc acc (leaf-chunk ρ)))]
          [else (loop (node-right ρ) (loop (node-left ρ) acc))])))

;; Return the rope's chunks as a list, ordered left-to-right.
(define (rope-chunks ρ)
  (reverse (rope-fold-chunks (λ (acc c) (cons c acc)) null ρ)))

;; ---------------------------------------------------------------------------------------
;; Structural statistics and validation -- diagnostics only
;; ---------------------------------------------------------------------------------------
;;
;; `rope-leaf-count` / `rope-node-count`: diagnostic counters used by tests and benchmarks.
;;
;; `rope-valid?` checks all structural invariants. It verifies that the tree is balanced,
;; every cached measure is correct, non-empty leaves respect the size cap, and the only
;; zero-length rope is the unique `empty-rope` value. Run it in tests after every random
;; operation; a failure means a bug in join, split, slice or concat.

(define (rope-leaf-count ρ)
  (if (leaf? ρ)
      (if (= (leaf-length ρ) 0) 0 1)
      (+ (rope-leaf-count (node-left ρ)) (rope-leaf-count (node-right ρ)))))

(define (rope-node-count ρ)
  (if (leaf? ρ) 0
      (add1 (+ (rope-node-count (node-left ρ)) (rope-node-count (node-right ρ))))))

;; `#t` when every structural invariant holds:
;;   - cached length / width / height on nodes are correct
;;   - sibling heights differ by at most one (AVL balance)
;;   - non-empty leaves hold at most `limit` elements
;;   - the only zero-length rope is the unique `empty-rope` at the root
;;
;; The `top?` flag distinguishes the root (which may legitimately be `empty-rope`) from an
;; empty leaf elsewhere in the tree (which is an error).
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
