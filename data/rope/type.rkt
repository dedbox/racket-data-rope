#lang racket/base

(require (for-syntax data/rope/private/stxclasses
                     data/rope/private/type
                     racket/base
                     racket/syntax
                     syntax/parse)
         data/rope/core
         syntax/parse/define)

(provide define-rope-type)

(define-syntax-parse-rule (define-rope-type τ:id
                            #:chunk? chunk?:id+fun1
                            #:empty empty:lit+id+fun0
                            #:length len:id+fun1
                            #:ref ref:id+fun2
                            #:slice slice:id+fun3
                            #:append app:id+fun*+fun-rest
                            #:width width:nat+id+fun2
                            #:limit limit:nat
                            #:merge-limit merge-limit:nat)
  #:fail-when (or (< (syntax-e #'merge-limit) 1)
                  (> (syntax-e #'merge-limit) (syntax-e #'limit)))
  "merge-limit must be between 1 and limit"
  #:with (~var τ:dict) (format-id #'τ "~a:dict" #'τ)

  (begin
    (define τ:dict (make-chunk-dict #:empty empty #:length len #:ref ref #:slice slice
                                    #:append app #:width width #:limit limit
                                    #:merge-limit merge-limit))
    ;; Record `τ` with nine slots in `type-keys` order, `chunk?` last (notes/m1.org §4).
    (define-syntax τ
      (rope-type 'τ #'τ:dict
                 (list empty len ref slice app width limit merge-limit chunk?)))))
