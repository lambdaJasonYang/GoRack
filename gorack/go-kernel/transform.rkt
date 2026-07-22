#lang racket/base

;; Stable protocol for transformations over the generated Go kernel. Passes
;; name the official node kinds whose semantics they understand. Everything
;; else is either traversed and retained generically (the normal policy) or
;; rejected explicitly by a pass that cannot be sound without understanding
;; every node kind.

(require racket/contract
         racket/list
         "node.rkt"
         "schema.rkt")

(define unknown-policies '(recurse reject))

(define (pass-name? value)
  (or (and (string? value) (not (string=? value "")))
      (symbol? value)))

(define (handled-kinds? value)
  (and (list? value)
       (andmap (lambda (kind) (and (string? kind) (not (string=? kind "")))) value)
       (= (length value) (length (remove-duplicates value string=?)))))

(define (pass-handler? value)
  (and (procedure? value) (procedure-arity-includes? value 2)))

(struct go-kernel-pass (name handled-kinds unknown-policy handler)
  #:transparent
  #:guard
  (lambda (name handled-kinds unknown-policy handler who)
    (unless (pass-name? name)
      (raise-argument-error who "(or/c non-empty-string? symbol?)" name))
    (unless (handled-kinds? handled-kinds)
      (raise-argument-error who "(listof distinct non-empty-string?)" handled-kinds))
    (unless (memq unknown-policy unknown-policies)
      (raise-argument-error who "(or/c 'recurse 'reject)" unknown-policy))
    (unless (pass-handler? handler)
      (raise-argument-error who "(procedure-arity-includes/c 2)" handler))
    (values name handled-kinds unknown-policy handler)))

(struct exn:fail:gorack:transform exn:fail
  (pass node-id kind)
  #:transparent)

(define (go-kernel-pass-handles? pass kind)
  (and (member kind (go-kernel-pass-handled-kinds pass) string=?) #t))

(define (raise-transform-error pass node message)
  (raise
   (exn:fail:gorack:transform
    message
    (current-continuation-marks)
    (go-kernel-pass-name pass)
    (ir-node-id node)
    (ir-node-kind node))))

(define (run-go-kernel-pass pass root #:context [context #f])
  (unless (go-kernel-pass? pass)
    (raise-argument-error 'run-go-kernel-pass "go-kernel-pass?" pass))
  (unless (ir-node? root)
    (raise-argument-error 'run-go-kernel-pass "ir-node?" root))
  (ir-node-map
   root
   (lambda (node)
     (cond
       [(not (string=? (ir-node-namespace node) "go/ast")) node]
       [(go-kernel-pass-handles? pass (ir-node-kind node))
        (define transformed ((go-kernel-pass-handler pass) node context))
        (unless (ir-node? transformed)
          (raise-transform-error pass node "transformation handler did not return an IR node"))
        (unless (string=? (ir-node-id transformed) (ir-node-id node))
          (raise-transform-error pass node "transformation handler changed the existing node ID"))
        transformed]
       [(eq? (go-kernel-pass-unknown-policy pass) 'recurse)
        ;; ir-node-map has already transformed children bottom-up. Keeping this
        ;; node therefore preserves every unknown field and official node kind.
        node]
       [else
        (raise-transform-error
         pass node
         (format "pass ~a does not declare handling for go/ast.~a"
                 (go-kernel-pass-name pass)
                 (ir-node-kind node)))]))))

(define (go-kernel-pass-unhandled-kinds pass [schema #f])
  (define schema* (or schema (current-go-schema) (load-go-schema)))
  (for/list ([node (in-list (go-schema-nodes schema*))]
             #:unless (go-kernel-pass-handles? pass (go-node-schema-name node)))
    (go-node-schema-name node)))

(define (audit-go-kernel-passes passes #:schema [schema #f])
  (define schema* (or schema (current-go-schema) (load-go-schema)))
  (for/list ([pass (in-list passes)])
    (unless (go-kernel-pass? pass)
      (raise-argument-error 'audit-go-kernel-passes
                            "(listof go-kernel-pass?)" passes))
    (hasheq 'pass (format "~a" (go-kernel-pass-name pass))
            'unknownPolicy (symbol->string (go-kernel-pass-unknown-policy pass))
            'unhandledKinds (go-kernel-pass-unhandled-kinds pass schema*))))

(provide
 (struct-out exn:fail:gorack:transform)
 (contract-out
  [struct go-kernel-pass
    ([name pass-name?]
     [handled-kinds handled-kinds?]
     [unknown-policy (or/c 'recurse 'reject)]
     [handler pass-handler?])]
  [go-kernel-pass-handles? (-> go-kernel-pass? string? boolean?)]
  [run-go-kernel-pass
   (->* (go-kernel-pass? ir-node?) (#:context any/c) ir-node?)]
  [go-kernel-pass-unhandled-kinds
   (->* (go-kernel-pass?) ((or/c go-schema? #f)) (listof string?))]
  [audit-go-kernel-passes
   (->* ((listof go-kernel-pass?)) (#:schema (or/c go-schema? #f)) list?)]))

(module+ test
  (require rackunit)
  (define child
    (make-ir-node "n2" "go/ast" "Ident" '( ("Name" . "before") )))
  (define root
    (make-ir-node "n1" "go/ast" "FutureExpr" `(("Child" . ,child))))
  (define recurse-pass
    (go-kernel-pass
     'rename-ident
     '("Ident")
     'recurse
     (lambda (node _context)
       (ir-node-set-field node "Name" "after"))))
  (define transformed (run-go-kernel-pass recurse-pass root))
  (check-equal? (ir-node-kind transformed) "FutureExpr")
  (check-equal? (ir-node-field (ir-node-field transformed "Child") "Name") "after")
  (check-equal? (ir-node-id (ir-node-field transformed "Child")) "n2")

  (define strict-pass
    (go-kernel-pass 'semantic-check '("Ident") 'reject (lambda (node _) node)))
  (check-exn exn:fail:gorack:transform?
             (lambda () (run-go-kernel-pass strict-pass root)))
  (check-exn exn:fail:gorack:transform?
             (lambda ()
               (run-go-kernel-pass
                (go-kernel-pass 'bad-result '("Ident") 'recurse
                                   (lambda (_node _) #f))
                child))))
