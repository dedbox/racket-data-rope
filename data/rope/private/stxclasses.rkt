#lang racket/base

(require syntax/parse
         (for-template racket/base))

(provide (all-defined-out))

(define-syntax-class vector-lit
  #:description "a vector literal"
  #:opaque
  (pattern x #:when (vector? (syntax-e #'x))))

(define-syntax-class hash-lit
  #:description "a hash literal"
  #:opaque
  (pattern x #:when (hash? (syntax-e #'x))))

(define-syntax-class box-lit
  #:description "a box literal"
  #:opaque
  (pattern x #:when (box? (syntax-e #'x))))

(define-syntax-class prefab-lit
  #:description "a prefab struct literal"
  #:opaque
  (pattern x #:when (struct? (syntax-e #'x))))

(define-syntax-class self-quoting-lit
  #:description "a self-quoting literal"
  #:opaque
  ;; built-in syntax classes
  (pattern (~or :number :boolean :string :bytes :char :regexp :byte-regexp))
  ;; custom syntax classes
  (pattern (~or :vector-lit :hash-lit :box-lit :prefab-lit)))

(define-syntax-class quotable-lit
  #:description "a quotable literal"
  #:opaque
  (pattern (~or :self-quoting-lit :keyword :id)))

(define-syntax-class lit
  #:description "a literal value"
  #:opaque
  #:literals (quote)
  ;; self-quoting atomic literals
  (pattern :self-quoting-lit)
  ;; quoted literals
  (pattern (quote (~or :quotable-lit (:quotable-lit ...)))))

(define-syntax-class fun-rest
  #:description "a variable-arity function"
  #:opaque
  #:literals (lambda λ)
  (pattern ((~or lambda λ) (~and args (~or :id (_ ...+ . rest))) . _)
           #:do [(define args-datum (syntax->datum #'args))]
           #:when (and (pair? args-datum) (not (list? args-datum)))))

;; function literals whose argument list is fixed
(define-syntax-class fun*
  #:description "a function of any fixed arity"
  #:opaque
  #:literals (lambda λ)
  (pattern ((~or lambda λ) (fixed:id ...) . _)
           #:attr arity (length (attribute fixed))))

(define-syntax-class id+fun*+fun-rest
  #:description "an identifier, a fixed-arity function, or a variable-arity function"
  #:opaque
  (pattern (~or :id :fun* :fun-rest)))

(define-syntax-class (fun arity)
  #:description "a zero-argument function"
  #:opaque
  #:literals (lambda λ)
  (pattern ((~or lambda λ) (args:id ...) . _)
           #:when (= arity (length (attribute args)))))

(define-syntax-class id+fun1
  #:description "an identifier or a one-argument function"
  #:opaque
  (pattern (~or :id (~var _ (fun 1)))))

(define-syntax-class id+fun2
  #:description "an identifier or a two-argument function"
  #:opaque
  (pattern (~or :id (~var _ (fun 2)))))

(define-syntax-class id+fun3
  #:description "an identifier or a three-argument function"
  #:opaque
  (pattern (~or :id (~var _ (fun 3)))))

(define-syntax-class id+fun5
  #:description "an identifier or a five-argument function"
  #:opaque
  (pattern (~or :id (~var _ (fun 5)))))

(define-syntax-class nat+id+fun0
  #:description "a natural number, an identifier, or a zero-argument function"
  #:opaque
  #:attributes (callable)
  (pattern n:nat
           #:attr callable #'(λ () n))
  (pattern (~and callable (~or :id (~var _ (fun 0))))))

(define-syntax-class nat+id+fun2
  #:description "a natural number, an identifier, or a two-argument function"
  #:opaque
  #:attributes (callable)
  (pattern n:nat
           #:attr callable #'(λ (_x _y) n))
  (pattern (~and callable (~or :id (~var _ (fun 2))))))

(define-syntax-class lit+id+fun0
  #:description "a literal value, an identifier, or a zero-argument function"
  #:opaque
  #:attributes (callable)
  (pattern l:lit
           #:attr callable #'(λ () l))
  (pattern (~and callable (~or :id (~var _ (fun 0))))))
