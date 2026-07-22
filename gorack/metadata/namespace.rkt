#lang racket/base

(require racket/contract)

;; Namespace identifiers are part of the stable metadata envelope. They are
;; independent from any particular higher-level language or lowering registry.
(define namespace-pattern
  #px"^[A-Za-z][A-Za-z0-9_-]*(?:[./][A-Za-z0-9][A-Za-z0-9_-]*)*$")

(define (namespace-id? value)
  (and (string? value)
       (regexp-match? namespace-pattern value)
       #t))

(define (go-ast-namespace? value)
  (equal? value "go/ast"))

(define (extension-namespace? value)
  (and (namespace-id? value)
       (not (go-ast-namespace? value))))

(provide
 (contract-out
  [namespace-id? (-> any/c boolean?)]
  [go-ast-namespace? (-> any/c boolean?)]
  [extension-namespace? (-> any/c boolean?)]))

(module+ test
  (require rackunit)
  (check-true (namespace-id? "gorack.types.v1"))
  (check-true (namespace-id? "go/ast"))
  (check-false (namespace-id? "not a namespace"))
  (check-true (go-ast-namespace? "go/ast"))
  (check-false (extension-namespace? "go/ast"))
  (check-true (extension-namespace? "gorack.types.v1")))
