#lang racket/base

;;; tests/type-tests.rkt -- unit tests for data/rope/type.rkt.
;;;
;;; `define-rope-type` turns one declaration into a pair of bindings in the declaring
;;; module: a runtime `τ:dict` dictionary and a syntax-level `τ` record. The roles are
;;; read in different phases, so each gets its own kind of test -- the dictionary at
;;; runtime, the record during expansion.
;;;
;;; === What the tests assert ===
;;;
;;; - `τ:dict` carries the declared limits, empty chunk, and primitives
;;; - a constant `#:width` becomes a procedure that scales by that constant
;;; - a `#:width` identifier is passed through unwrapped
;;; - an inline lambda is arity-checked against the position it fills
;;; - `τ` resolves to nine values in `type-keys` order, with `chunk?` last
;;; - the record stores what the declaration evaluates, not the source syntax
;;; - the type name is a syntax binding, so it cannot be used as a value
;;; - two declarations produce distinct dictionaries and distinct records
;;; - `#:merge-limit` sits between 1 and `#:limit`, inclusive at both ends
;;; - `#:chunk?` is required, and the dictionary never receives it
;;; - a compile-time failure is caught by compiling a throwaway module in this process

(module+ test
  (require rackunit
           rackunit/text-ui
           (for-syntax racket/base
                       data/rope/private/type-stx)
           data/rope/core
           data/rope/private/type-stx
           data/rope/type
           syntax/parse/define)

  ;; =====================================================================
  ;; Helpers
  ;; =====================================================================

  ;; Classify one recorded primitive. The record is built in the transformer phase, so a
  ;; primitive is whatever the declaration evaluates there, never the source syntax.
  (define-for-syntax (tag p)
    (cond [(syntax? p) 'syntax]
          [(procedure? p) 'proc]
          [(number? p) 'num]
          [(string? p) 'str]
          [else 'other]))

  ;; Expand to the record behind `τ` as data. The record lives in the binding space, which
  ;; is readable during expansion only, so the assertion compares datums.
  (define-syntax-parse-rule (record-of τ:id)
    #:do [(define tp (resolve-rope-type #'τ))
          (define tagged (map tag (rope-type-primitives tp)))]
    #:with out (datum->syntax #'τ
                               (list 'quote (list (rope-type-name tp)
                                                  (syntax->datum (rope-type-dict-id tp))
                                                  tagged))
                               #'τ)
    out)

  ;; Expand to `#t` when `a` and `b` resolve to one record rather than two.
  (define-syntax-parse-rule (same-record? a:id b:id)
    #:do [(define tp (resolve-rope-type #'a))
          (define tq (resolve-rope-type #'b))]
    #:with out (datum->syntax #'a (eq? tp tq) #'a)
    out)

  ;; Wrap `forms` in a throwaway module that requires the type layer. The module is
  ;; compiled inside this process, which is how a compile-time failure is made catchable:
  ;; a bad declaration written here would stop the whole test file from compiling.
  (define (snippet forms)
    `(module declared-type racket/base
       (require (for-syntax racket/base syntax/parse data/rope/private/type-stx)
                syntax/parse/define
                data/rope/core
                data/rope/private/type-stx
                data/rope/type)
       ,@forms))

  ;; Compile `src` as its own module inside this process. Return the failure message, or
  ;; `#f` when the module compiles.
  (define (compile-message src)
    (with-handlers ([exn:fail:syntax? (λ (e) (exn-message e))]
                    [exn:fail:contract? (λ (e) (exn-message e))])
      (eval (datum->syntax #'compile-message src))
      #f))

  ;; Assert that compiling `src` fails with a message matching `rx`. `regexp-match`
  ;; returns the match list, so the assertion is non-#f rather than `#t`.
  (define (check-fails src rx what)
    (define msg (compile-message src))
    (check-not-false (and msg (regexp-match rx msg))
                     what))

  ;; =====================================================================
  ;; Declarations
  ;; =====================================================================

  ;; A string type whose width is the constant 1: eight elements per leaf, merged at 4.
  (define-rope-type str-type
    #:chunk? string? #:empty "" #:length string-length #:ref string-ref
    #:slice substring #:append string-append #:width 1
    #:limit 8 #:merge-limit 4)

  ;; A bytes type whose width comes from an identifier, at the tightest legal merge-limit.
  (define-rope-type bytes-type
    #:chunk? bytes? #:empty #"" #:length bytes-length #:ref bytes-ref
    #:slice subbytes #:append bytes-append #:width bytes-length
    #:limit 4 #:merge-limit 1)

  ;; Two indices per element, and `#:merge-limit` exactly at `#:limit`.
  (define-rope-type wide-type
    #:chunk? string? #:empty "" #:length string-length #:ref string-ref
    #:slice substring #:append string-append #:width 2
    #:limit 8 #:merge-limit 8)

  ;; Inline lambdas stand in for the primitives, so each position is arity-checked.
  ;; `#:append` accepts a fixed-arity lambda as well as a variadic one; the core calls it
  ;; with two arguments, so the two-argument shape is the one that runs.
  (define-rope-type fn-type
    #:chunk? string? #:empty "" #:length (λ (c) (string-length c))
    #:ref (λ (c i) (string-ref c i)) #:slice (λ (c i j) (substring c i j))
    #:append (λ (a b) (string-append a b)) #:width 1
    #:limit 8 #:merge-limit 4)

  ;; Keyword arguments for a legal string type, without the two limit pairs.
  (define base-kw '(#:chunk? string? #:empty "" #:length string-length #:ref string-ref
                    #:slice substring #:append string-append #:width 1))

  ;; Drop the pair that follows `key`, or replace its value. Pairs are prepended value
  ;; first, so the reversal restores each keyword ahead of its value.
  (define (drop-kw kw key)
    (let loop ([k kw] [out null])
      (cond [(null? k) (reverse out)]
            [(eq? (car k) key) (loop (cddr k) out)]
            [else (loop (cddr k) (cons (cadr k) (cons (car k) out)))])))

  (define (replace-kw kw key value)
    (let loop ([k kw] [out null])
      (cond [(null? k) (reverse out)]
            [(eq? (car k) key) (loop (cddr k) (cons value (cons key out)))]
            [else (loop (cddr k) (cons (cadr k) (cons (car k) out)))])))

  ;; A module that declares one legal string type, with the limits and `kw` adjusted.
  ;; `#:limit` and `#:merge-limit` are appended, so a test that drops one of them has to
  ;; build the module itself.
  (define (declaring #:kw [kw base-kw] #:limit [limit 8] #:merge-limit [merge-limit 4])
    (snippet (list (cons 'define-rope-type
                          (cons 't (append kw (list '#:limit limit '#:merge-limit merge-limit)))))))

  ;; =====================================================================
  ;; The dictionary a declaration builds
  ;; =====================================================================

  (define suite-dictionary
    (test-suite "the dictionary behind τ:dict"
      (test-case "the two limits are the declared values"
        (check-equal? (chunk-dict-limit str-type:dict) 8)
        (check-equal? (chunk-dict-merge-limit str-type:dict) 4)
        (check-equal? (chunk-dict-limit bytes-type:dict) 4)
        (check-equal? (chunk-dict-merge-limit bytes-type:dict) 1)
        (check-equal? (chunk-dict-limit wide-type:dict) 8)
        (check-equal? (chunk-dict-merge-limit wide-type:dict) 8))
      (test-case "a constant width becomes a procedure that scales the element count"
        (check-true (procedure? (chunk-dict-width str-type:dict)))
        (check-equal? ((chunk-dict-width str-type:dict) "abc") 3)
        (check-equal? ((chunk-dict-width wide-type:dict) "abc") 6))
      (test-case "an identifier width is stored unwrapped"
        ;; bytes-type declares #:width bytes-length, so width equals length here.
        (check-eq? (chunk-dict-width bytes-type:dict) bytes-length)
        (check-equal? ((chunk-dict-width bytes-type:dict) #"abcd") 4))
      (test-case "the empty chunk is the declared value"
        (check-equal? (chunk-dict-empty str-type:dict) "")
        (check-equal? (chunk-dict-empty bytes-type:dict) #"")
        (check-eq? (make-leaf str-type:dict (chunk-dict-empty str-type:dict)) empty-rope))
      (test-case "the operation fields are the declared primitives"
        (check-equal? ((chunk-dict-ref str-type:dict) "abc" 1) #\b)
        (check-equal? ((chunk-dict-slice str-type:dict) "abcdef" 1 4) "bcd")
        (check-equal? ((chunk-dict-append str-type:dict) "ab" "cd") "abcd")
        (check-equal? ((chunk-dict-ref bytes-type:dict) #"abc" 1) (char->integer #\b)))
      (test-case "inline lambdas fill the same positions as identifiers"
        (check-equal? ((chunk-dict-length fn-type:dict) "abcd") 4)
        (check-equal? ((chunk-dict-ref fn-type:dict) "abc" 1) #\b)
        (check-equal? ((chunk-dict-slice fn-type:dict) "abcdef" 1 4) "bcd")
        (check-equal? ((chunk-dict-append fn-type:dict) "ab" "cd") "abcd")
        (check-equal? ((chunk-dict-width fn-type:dict) "abcd") 4))
      (test-case "a rope built from the dictionary is valid and bounded"
        (define ρ (chunk->rope str-type:dict "abcdefghij"))
        (check-true (rope-valid? str-type:dict ρ))
        (check-equal? (rope-length ρ) 10)
        (check-equal? (map string-length (rope-chunks ρ)) (list 8 2))
        (check-equal? (rope-width ρ) 10)
        ;; wide-type spans two indices per element, so width is twice the element count.
        (check-equal? (rope-width (chunk->rope wide-type:dict "abcde")) 10))
      (test-case "concat merges boundary leaves within merge-limit"
        ;; rope-concat splices the two boundary leaves when their combined element count
        ;; is at most merge-limit, which is below limit for str-type.
        (define σ str-type:dict)
        (define merged (rope-concat σ (chunk->rope σ "ab") (chunk->rope σ "cd")))
        (check-equal? (map string-length (rope-chunks merged)) (list 4))
        (check-true (rope-valid? σ merged))
        (define unmerged (rope-concat σ (chunk->rope σ "abcd") (chunk->rope σ "abcd")))
        (check-equal? (map string-length (rope-chunks unmerged)) (list 4 4))
        (check-equal? (rope-length unmerged) 8)
        (check-true (rope-valid? σ unmerged)))
      (test-case "a width lambda the core cannot call is declared, then fails on use"
        ;; #:width accepts a constant, an identifier, or a two-argument function, while
        ;; chunk->rope calls width with one argument. The lambda shape is accepted at
        ;; declaration and breaks at the first leaf.
        (define-rope-type lambda-width-type
          #:chunk? string? #:empty "" #:length string-length #:ref string-ref
          #:slice substring #:append string-append #:width (λ (a b) 1)
          #:limit 8 #:merge-limit 4)
        (check-equal? (chunk-dict-limit lambda-width-type:dict) 8)
        (check-exn #rx"arity mismatch"
                   (λ () (chunk->rope lambda-width-type:dict "abcdef"))))
      (test-case "a one-argument append lambda is declared, then fails on use"
        ;; #:append takes a function of any arity, so the declaration expands, but append
        ;; is called with two arguments and the first merge raises an arity mismatch.
        (define-rope-type one-arg-append-type
          #:chunk? string? #:empty "" #:length string-length #:ref string-ref
          #:slice substring #:append (λ (c) (string-append c c)) #:width 1
          #:limit 8 #:merge-limit 4)
        (check-exn #rx"arity mismatch"
                   (λ () ((chunk-dict-append one-arg-append-type:dict) "ab" "cd"))))
      (test-case "an empty-chunk function is stored as the chunk value"
        ;; The width and empty classes carry a callable attribute that the declaration
        ;; never uses, so a zero-argument function lands in the dictionary verbatim.
        (define-rope-type thunk-empty-type
          #:chunk? string? #:empty (λ () "") #:length string-length #:ref string-ref
          #:slice substring #:append string-append #:width 1
          #:limit 8 #:merge-limit 4)
        (check-true (procedure? (chunk-dict-empty thunk-empty-type:dict))))))

  ;; =====================================================================
  ;; The record behind the type name
  ;; =====================================================================

  (define suite-record
    (test-suite "the record behind τ"
      (test-case "the record carries the type name and the dictionary identifier"
        (check-equal? (car (record-of str-type)) 'str-type)
        (check-equal? (cadr (record-of str-type)) 'str-type:dict)
        (check-equal? (car (record-of bytes-type)) 'bytes-type)
        (check-equal? (cadr (record-of bytes-type)) 'bytes-type:dict))
      (test-case "the primitives fill the type-keys slots, chunk? last"
        ;; Slot 8 is #:chunk?, the compile-time predicate the dictionary has no field
        ;; for, and slots 0 through 7 line up with the eight dictionary fields.
        (check-equal? (caddr (record-of str-type))
                      '(str proc proc proc proc num num num proc))
        (check-equal? (length (caddr (record-of str-type))) (length type-keys))
        (check-equal? (caddr (record-of fn-type))
                      '(str proc proc proc proc num num num proc)))
      (test-case "the record stores values, not the source syntax"
        ;; The record is built in the transformer phase, so each primitive is the value
        ;; the declaration evaluates there. A source form cannot be recovered from it.
        (check-false (memq 'syntax (caddr (record-of str-type)))
                    "no syntax objects recorded")
        (check-false (memq 'syntax (caddr (record-of fn-type)))
                    "no syntax objects recorded for inline lambdas"))
      (test-case "two declarations resolve to two records"
        (check-true (same-record? str-type str-type))
        (check-false (same-record? str-type bytes-type))
        (check-false (eq? str-type:dict bytes-type:dict))
        (check-false (eq? (cadr (record-of str-type)) (cadr (record-of bytes-type)))))
      (test-case "the type name is a syntax binding, not a value"
        (check-fails (snippet (list (cons 'define-rope-type
                                          (cons 't (append base-kw (list '#:limit 8
                                                          '#:merge-limit 4))))
                                    (list 'void 't)))
                     #rx"illegal use of syntax"
                     "τ cannot be read as a value"))))

  ;; =====================================================================
  ;; The limit bound, checked while the declaration expands
  ;; =====================================================================

  (define suite-bounds
    (test-suite "the merge-limit bound"
      (test-case "merge-limit is accepted at both ends of the range"
        (check-equal? (compile-message (declaring #:limit 8 #:merge-limit 1)) #f)
        (check-equal? (compile-message (declaring #:limit 8 #:merge-limit 8)) #f))
      (test-case "merge-limit above limit is refused"
        (check-fails (declaring #:limit 8 #:merge-limit 9)
                     #rx"merge-limit must be between 1 and limit"
                     "merge-limit above limit"))
      (test-case "merge-limit below 1 is refused"
        (check-fails (declaring #:limit 8 #:merge-limit 0)
                     #rx"merge-limit must be between 1 and limit"
                     "merge-limit below 1"))
      (test-case "limit 0 is refused through the merge-limit message"
        ;; #:limit accepts 0, so the bound below it is what rejects the declaration, and
        ;; the message names merge-limit even though limit is the offending pair.
        (check-fails (declaring #:limit 0 #:merge-limit 0)
                     #rx"merge-limit must be between 1 and limit"
                     "limit 0"))))

  ;; =====================================================================
  ;; The grammar each position accepts
  ;; =====================================================================

  (define suite-grammar
    (test-suite "what a declaration may say"
      (test-case "chunk?, limit, and merge-limit are required keywords"
        (check-fails (declaring #:kw (drop-kw base-kw '#:chunk?))
                     #rx"expected the literal #:chunk\\?" "chunk? is required")
        (define full-kw (append base-kw (list '#:limit 8 '#:merge-limit 4)))
        (check-fails (snippet (list (cons 'define-rope-type
                                          (cons 't (drop-kw full-kw '#:limit)))))
                     #rx"expected the literal #:limit" "limit is required")
        (check-fails (snippet (list (cons 'define-rope-type
                                          (cons 't (drop-kw full-kw '#:merge-limit)))))
                     #rx"expected more terms starting with the literal #:merge-limit"
                     "merge-limit is required"))
      (test-case "width rejects a one-argument lambda but accepts a two-argument one"
        ;; chunk->rope calls width with one argument, so the rejected shape is the one the
        ;; core can use and the accepted shape is the one it cannot.
        (check-fails (declaring #:kw (replace-kw base-kw '#:width '(λ (c) (string-length c))))
                     #rx"two-argument function" "one-argument width lambda")
        (check-equal? (compile-message (declaring #:kw (replace-kw base-kw '#:width '(λ (a b) 1))))
                      #f))
      (test-case "empty rejects a chunk built by an expression"
        ;; A vector literal is not self-quoting, so the vector dictionary's empty chunk
        ;; cannot be declared as a literal.
        (check-fails (declaring #:kw (replace-kw base-kw '#:empty '(vector)))
                     #rx"a literal value, an identifier, or a zero-argument function"
                     "empty chunk as an expression"))
      (test-case "an operation position checks its lambda's arity"
        (check-fails (declaring #:kw (replace-kw base-kw '#:ref '(λ (c) (string-ref c 0)))
                                #:merge-limit 4)
                     #rx"two-argument function" "ref lambda arity"))
      (test-case "append accepts an identifier or a function literal of any arity"
        ;; #:append is a three-way disjunction: an identifier, a lambda whose argument
        ;; list is fixed, and a variadic lambda. Only a non-function value is refused.
        (define append-2 '(λ (a b) (string-append a b)))
        (define append-1 '(λ (c) (string-append c c)))
        (define append-0 '(λ () ""))
        (define append-rest '(λ (a b . rest)
                               (apply string-append (cons a (cons b rest)))))
        (check-equal? (compile-message (declaring #:kw base-kw)) #f)
        (check-equal? (compile-message
                       (declaring #:kw (replace-kw base-kw '#:append append-2)))
                       #f)
        (check-equal? (compile-message
                       (declaring #:kw (replace-kw base-kw '#:append append-1)))
                       #f)
        (check-equal? (compile-message
                       (declaring #:kw (replace-kw base-kw '#:append append-0)))
                       #f)
        (check-equal? (compile-message
                       (declaring #:kw (replace-kw base-kw '#:append append-rest)))
                       #f)
        (check-fails (declaring #:kw (replace-kw base-kw '#:append 5))
                     #rx"expected an identifier, a fixed-arity function, or a variable-arity"
                     "non-function append value"))
      (test-case "the limits must be exact natural numbers"
        (check-fails (declaring #:limit -5 #:merge-limit 4)
                     #rx"exact-nonnegative-integer" "negative limit")
        (check-fails (declaring #:limit 3/2 #:merge-limit 2)
                     #rx"exact-nonnegative-integer" "fractional limit"))
      (test-case "the type name must be an identifier"
        (check-fails (snippet (list (cons 'define-rope-type
                                          (cons 5 (append base-kw (list '#:limit 8 '#:merge-limit 4))))))
                     #rx"expected identifier" "non-identifier type name"))
      (test-case "one module declares a name once"
        (check-fails (snippet (list (cons 'define-rope-type
                                          (cons 't (append base-kw (list '#:limit 8 '#:merge-limit 4))))
                                    (cons 'define-rope-type
                                          (cons 't (append base-kw (list '#:limit 16 '#:merge-limit 8))))))
                     #rx"already defined" "duplicate declaration"))
      (test-case "the primitives need no transformer-phase binding"
        ;; Every position matches an identifier without looking up its binding, so a
        ;; module that requires only the runtime layer still expands the declaration.
        (define bare
          '(module bare-type racket/base
             (require data/rope/type)
             (define-rope-type t #:chunk? string? #:empty ""
                               #:length string-length #:ref string-ref
                               #:slice substring #:append string-append
                               #:width 1 #:limit 8 #:merge-limit 4)))
        (check-equal? (compile-message bare) #f))
      (test-case "the dictionary name is a local binding of the declaring module"
        ;; define-rope-type defines τ:dict but provides nothing, so the declaring module
        ;; can read it and must arrange its own provide clause. τ itself is a syntax
        ;; binding, so providing it works while reading it as a value does not.
        (define reads-dict
          '(module declares-and-reads racket/base
             (require data/rope/type)
             (define-rope-type t #:chunk? string? #:empty ""
                               #:length string-length #:ref string-ref
                               #:slice substring #:append string-append
                               #:width 1 #:limit 8 #:merge-limit 4)
             (void t:dict)))
        (define provides-dict
          '(module declares-and-provides racket/base
             (require data/rope/type)
             (define-rope-type t #:chunk? string? #:empty ""
                               #:length string-length #:ref string-ref
                               #:slice substring #:append string-append
                               #:width 1 #:limit 8 #:merge-limit 4)
             (provide t:dict)))
        (define provides-type
          '(module declares-and-provides-name racket/base
             (require data/rope/type)
             (define-rope-type t #:chunk? string? #:empty ""
                               #:length string-length #:ref string-ref
                               #:slice substring #:append string-append
                               #:width 1 #:limit 8 #:merge-limit 4)
             (provide t)))
        (define reads-type
          '(module declares-and-reads-type racket/base
             (require data/rope/type)
             (define-rope-type t #:chunk? string? #:empty ""
                               #:length string-length #:ref string-ref
                               #:slice substring #:append string-append
                               #:width 1 #:limit 8 #:merge-limit 4)
             (void t)))
        (check-equal? (compile-message reads-dict) #f)
        (check-equal? (compile-message provides-dict) #f)
        (check-equal? (compile-message provides-type) #f)
        (check-fails reads-type #rx"illegal use of syntax" "τ is not a value"))))

  ;; =====================================================================
  ;; What data/rope/type exports
  ;; =====================================================================

  (define suite-surface
    (test-suite "the exported names"
      (test-case "the syntax-layer names are not re-exported"
        ;; The macro is the only exported name, so τ:dict travels through the declaring
        ;; module's own provide clause.
        (check-false (dynamic-require 'data/rope/type 'rope-type (λ () #f)))
        (check-false (dynamic-require 'data/rope/type 'resolve-rope-type (λ () #f)))
        (check-false (dynamic-require 'data/rope/type 'type-keys (λ () #f))))))

  ;; =====================================================================
  ;; Run everything
  ;; =====================================================================

  (run-tests
    (test-suite "type"
      suite-dictionary
      suite-record
      suite-bounds
      suite-grammar
      suite-surface)))
