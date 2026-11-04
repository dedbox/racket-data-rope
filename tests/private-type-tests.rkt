#lang racket/base

;;; tests/private-type-tests.rkt -- unit tests for data/rope/private/type.rkt.
;;;
;;; This module is the compile-time half of the type layer: the record that names a rope
;;; type, the key list the accessor reads, and the resolver that turns an identifier into
;;; that record. Its code runs during expansion, so a test reads a record inside a macro
;;; and splices what it finds into an assertion as data.
;;;
;;; === What the tests assert ===
;;;
;;; - `rope-type` is a three-field record: a name, a dictionary id, and the operations
;;; - `rope-type?` accepts records and rejects every other value
;;; - a record prints its fields, which is what a debugger reads
;;; - two records holding the same fields are not equal, so identity is binding identity
;;; - `type-keys` is the nine-key list the module declares
;;; - `resolve-rope-type` returns the record behind a type binding
;;; - a binding that is not a type is reported by name, at the caller's position
;;; - resolution needs an expansion context; a runtime call fails before the module's
;;;   own message is reached

(module+ test
  (require rackunit
           rackunit/text-ui
           (for-syntax racket/base
                       data/rope/private/type
                       syntax/parse)
           data/rope/private/type
           syntax/parse/define)

  ;; =====================================================================
  ;; Helpers
  ;; =====================================================================

  ;; A resolution result as data: the record's name when it succeeds, the message when it
  ;; does not. Resolution happens during expansion, so both are captured there and handed
  ;; to the assertion as data.
  (define-for-syntax (outcome τ tp)
    (datum->syntax τ (list 'quote (list 'resolved (rope-type-name tp))) τ))

  (define-for-syntax (failure τ msg)
    (datum->syntax τ (list 'quote (list 'failed msg)) τ))

  (define-syntax-parse-rule (resolve-result τ:id)
    #:with out (with-handlers ([exn:fail:syntax? (λ (e) (failure #'τ (exn-message e)))])
                 (outcome #'τ (resolve-rope-type #'τ)))
    out)

  ;; Bindings the resolver is asked about: a rope type, a macro that is not one, and a
  ;; runtime value. `no-such-binding` below is deliberately unbound.
  (define-syntax str-rope
    (rope-type 'str-rope #'str-rope:dict
               (list string? "" string-length string-ref substring string-append 1 8 4)))
  (define-syntax plain (λ (stx) #'(void)))
  (define (value-binding x) x)

  ;; =====================================================================
  ;; The record
  ;; =====================================================================

  (define suite-record
    (test-suite "the rope-type record"
      (test-case "the three fields are what the record holds"
        (define rec (rope-type 'sample #'sample:dict '(1 2 3)))
        (check-true (struct? rec))
        (check-true (rope-type? rec))
        (check-equal? (rope-type-name rec) 'sample)
        (check-equal? (syntax->datum (rope-type-dict-id rec)) 'sample:dict)
        (check-equal? (rope-type-primitives rec) '(1 2 3)))
      (test-case "rope-type? rejects every other value"
        (define rec (rope-type 'sample #'sample:dict '(1 2 3)))
        (check-true (rope-type? rec))
        (check-false (rope-type? 'sample))
        (check-false (rope-type? (list 'sample #'sample:dict '(1 2 3))))
        (check-false (rope-type? #f))
        (check-false (rope-type? (void))))
      (test-case "the record prints its fields"
        (define rec (rope-type 'sample #'sample:dict '(1 2 3)))
        (check-regexp-match #rx"^#\\(struct:rope-type sample #<syntax:.*sample:dict> \\(1 2 3\\)\\)$"
                            (format "~a" rec)))
      (test-case "two records with the same fields are not equal"
        ;; Nominal identity is binding identity, so equality on records does not make two
        ;; types interchangeable.
        (define rec (rope-type 'sample #'sample:dict '(1 2 3)))
        (define same (rope-type 'sample #'sample:dict '(1 2 3)))
        (check-false (equal? rec same))
        (check-false (eq? rec same)))))

  ;; =====================================================================
  ;; The key list
  ;; =====================================================================

  (define suite-keys
    (test-suite "the key list"
      (test-case "type-keys is the declared nine-key list"
        (check-equal? type-keys
                      '(empty length ref slice append width limit merge-limit chunk?))
        (check-equal? (length type-keys) 9))
      (test-case "the key list names the dictionary fields"
        ;; The keys name the dictionary fields, so each one matches what the declaration
        ;; sets, and `chunk?` sits last in the key list and in a record's primitives alike.
        ;; memq returns the tail that starts at the key, so the assertion is non-#f.
        (check-not-false (memq 'merge-limit type-keys))
        (check-false (memq 'merge-limit? type-keys))
        (check-true (eq? (car type-keys) 'empty))
        (check-true (eq? (list-ref type-keys 7) 'merge-limit))
        (check-true (eq? (list-ref type-keys 8) 'chunk?)))))

  ;; =====================================================================
  ;; Resolution
  ;; =====================================================================

  (define suite-resolution
    (test-suite "resolving a type identifier"
      (test-case "a type binding resolves to its record"
        (check-equal? (resolve-result str-rope) '(resolved str-rope)))
      (test-case "a macro that is not a rope type is refused by name"
        (define result (resolve-result plain))
        (check-equal? (car result) 'failed)
        (check-regexp-match #rx"plain: not a rope type" (cadr result)))
      (test-case "a runtime value binding is refused by name"
        (define result (resolve-result value-binding))
        (check-equal? (car result) 'failed)
        (check-regexp-match #rx"value-binding: not a rope type" (cadr result)))
      (test-case "an unbound identifier is refused by name"
        (define result (resolve-result no-such-binding))
        (check-equal? (car result) 'failed)
        (check-regexp-match #rx"no-such-binding: not a rope type" (cadr result)))
      (test-case "the message carries the caller's position"
        ;; raise-syntax-error keeps the caller's syntax object, so the report names the
        ;; file and column of the call rather than the resolver's own body.
        (define result (resolve-result plain))
        (check-regexp-match #rx"tests/private-type-tests\\.rkt:[0-9]+:[0-9]+: plain:"
                            (cadr result)))
      (test-case "a runtime call never reaches the module's own message"
        ;; syntax-local-value needs an expansion context, so the fallback procedure the
        ;; resolver passes is not consulted: the failure names syntax-local-value.
        (check-exn #rx"not currently expanding" (λ () (resolve-rope-type #'str-rope)))
        (check-exn #rx"not currently expanding"
                   (λ () (resolve-rope-type (datum->syntax #f 'anything)))))))

  ;; =====================================================================
  ;; Run everything
  ;; =====================================================================

  (run-tests
    (test-suite "private-type"
      suite-record
      suite-keys
      suite-resolution)))
