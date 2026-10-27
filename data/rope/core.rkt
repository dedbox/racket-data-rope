#lang racket/base

(provide (all-defined-out))

;; len   : number of elements
;; width : number of valid indices spanning the elements
(struct leaf (length width chunk) #:authentic #:sealed)
(struct node (length width height left right) #:authentic #:sealed)

;; The empty rope
(define empty-rope (leaf 0 0 #f))

;; empty       : the empty chunk
;; length      : chunk → nat
;; ref         : chunk index → element
;; slice       : chunk start end → chunk
;; append      : chunk chunk → chunk
;; width       : chunk → nat
;; limit       : the largest chunk a leaf can hold
;; merge-limit : ≤ Σ boundary leaf lengths ⇒ merge
(struct chunk-sig (empty length ref slice append width limit merge-limit)
  #:authentic #:sealed)

(define (make-chunk-sig #:empty e #:length len #:ref ref #:slice slice #:append app
                        #:width [width 1] #:limit limit #:merge-limit [merge-limit limit])
  (chunk-sig e len ref slice app
             (if (procedure? width) width (λ (c) (* width (len c))))
             limit merge-limit))

;; ---------------------------------------------------------------------------------------

(define (rope? x) (or (leaf? x) (node? x)))
(define (rope-length ρ) (if (leaf? ρ) (leaf-length ρ) (node-length ρ)))
(define (rope-width  ρ) (if (leaf? ρ) (leaf-width  ρ) (node-width  ρ)))
(define (rope-height ρ) (if (leaf? ρ) 0 (node-height ρ)))
(define (rope-empty? ρ) (= (rope-length ρ) 0))

(define (node-children ρ)
  (values (node-left ρ) (node-right ρ)))

;; ---------------------------------------------------------------------------------------

(define (make-node l r)
  (node (+ (rope-length l) (rope-length r))
        (+ (rope-width l) (rope-width r))
        (add1 (max (rope-height l) (rope-height r)))
        l r))

(define (make-leaf σ c)
  (define n ((chunk-sig-length σ) c))
  (if (= n 0) empty-rope (leaf n ((chunk-sig-width σ) c) c)))

;; ---------------------------------------------------------------------------------------

(define (chunk->rope σ c)
  (define n ((chunk-sig-length σ) c))
  (define limit (chunk-sig-limit σ))
  (define slice (chunk-sig-slice σ))
  (let build ([i 0] [j n])
    (define Δ (- j i))
    (cond [(= Δ 0) empty-rope]
          [(<= Δ limit) (make-leaf σ (if (and (= i 0) (= j n)) c (slice c i j)))]
          [else
           (define P (quotient (+ Δ (sub1 limit)) limit)) ; ⌈Δ/limit⌉ (span cardinality)
           (define k (+ i (* (quotient P 2) limit)))      ; i + ⌊P/2⌋·limit (split point)
           (make-node (build i k) (build k j))])))

;; ---------------------------------------------------------------------------------------
;; Tree Rotation
;; ---------------------------------------------------------------------------------------

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
;; Asymmetric Tree Join
;; ---------------------------------------------------------------------------------------

;; Moves down the right sub-tree of l until a node whose height is ≤ height(r) is found,
;; then merges r.
;;
;; Precondition: height(l) > height(r) + 1 ⇒ l is a node
(define (join-right l r)
  (define-values (ll lr) (node-children l))
  (cond [(<= (rope-height lr) (add1 (rope-height r)))
         (define ρ (make-node lr r))
         (if (<= (rope-height r) (add1 (rope-height ll)))
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

(define (rope-join l r)
  (define hl (rope-height l))
  (define hr (rope-height r))
  (cond [(> hl (add1 hr)) (join-right l r)]
        [(> hr (add1 hl)) (join-left l r)]
        [else (make-node l r)]))

;; ---------------------------------------------------------------------------------------

(define (first-leaf ρ) (if (leaf? ρ) ρ (first-leaf (node-left ρ))))
(define (last-leaf ρ) (if (leaf? ρ) ρ (last-leaf (node-right ρ))))

;; Preserves shape, so no rebalance.
(define (replace-last-leaf ρ r)
  (if (leaf? ρ) r (make-node (node-left ρ) (replace-last-leaf (node-right ρ) r))))

;; Rebalances when ρ is not a leaf.
(define (drop-first-leaf ρ)
  (cond [(leaf? ρ) empty-rope]
        [else
         (define l (drop-first-leaf (node-left ρ)))
         (if (eq? l empty-rope) (node-right ρ) (rope-join l (node-right ρ)))]))

;; ---------------------------------------------------------------------------------------

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

;; Returns the elements in [i, j).
(define (rope-slice σ ρ i j)
  (let slice ([ρ ρ] [i i] [j j])
    (cond [(= i j) empty-rope]
          [(and (= i 0) (= j (rope-length ρ))) ρ]
          [(leaf? ρ) (make-leaf σ ((chunk-sig-slice σ) (leaf-chunk ρ) i j))]
          [else
           (define-values (l r) (node-children ρ))
           (define n (rope-length l))
           (cond [(<= j n) (slice l i j)]
                 [(<= i n) (slice r (- i n) (- j n))]
                 [else (rope-join (slice l i n) (slice r 0 (- j n)))])])))

;; Replaces the elements in [i, j) with ρ*.
(define (rope-splice σ ρ i j ρ*)
  (define-values (prefix rest) (rope-split σ ρ i))
  (define-values (_dropped suffix) (rope-split σ rest (- j i)))
  (rope-concat σ (rope-concat σ prefix ρ*) suffix))

;; ---------------------------------------------------------------------------------------

(define (rope-ref σ ρ i)
  (let loop ([ρ ρ] [i i])
    (if (leaf? ρ)
        ((chunk-sig-ref σ) (leaf-chunk ρ) i)
        (let ([n (rope-length (node-left ρ))])
          (if (< i n)
              (loop (node-left ρ) i)
              (loop (node-right ρ) (- i n)))))))

(define (rope-fold-chunks proc init ρ)
  (let loop ([ρ ρ] [acc init])
    (cond [(leaf? ρ) (if (= (leaf-length ρ) 0) acc (proc acc (leaf-chunk ρ)))]
          [else (loop (node-right ρ) (loop (node-left ρ) acc))])))

(define (rope-chunks ρ)
  (reverse (rope-fold-chunks (lambda (acc c) (cons c acc)) null ρ)))

;; ---------------------------------------------------------------------------------------

(define (rope-leaf-count ρ)
  (if (leaf? ρ)
      (if (= (leaf-length ρ) 0) 0 1)
      (+ (rope-leaf-count (node-left ρ)) (rope-leaf-count (node-right ρ)))))

(define (rope-node-count ρ)
  (if (leaf? ρ) 0
      (add1 (+ (rope-node-count (node-left ρ)) (rope-node-count (node-right ρ))))))

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
           (and (check l #f)
                (check r #f)
                (= (node-length ρ) (+ (rope-length l) (rope-length r)))
                (= (node-width ρ) (+ (rope-width l) (rope-width r)))
                (= (node-height ρ) (add1 (max (rope-height l) (rope-height r))))
                (<= (abs (- (rope-height l) (rope-height r))) 1))])))
