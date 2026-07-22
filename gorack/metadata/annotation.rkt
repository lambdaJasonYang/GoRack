#lang racket/base

(require racket/contract
         "source.rkt"
         (only-in "../go-kernel/node.rkt" canonical-node-id)
         "namespace.rkt")

(struct annotation (namespace node-id value)
  #:transparent
  #:guard
  (lambda (namespace node-id value struct-name)
    (unless (namespace-id? namespace)
      (raise-argument-error struct-name "namespace-id?" namespace))
    (unless (stable-id? node-id)
      (raise-argument-error struct-name "stable-id?" node-id))
    (values namespace (canonical-node-id node-id) value)))

(define (make-annotation-table annotations)
  (for/fold ([table (hash)]) ([item (in-list annotations)])
    (define namespace (annotation-namespace item))
    (define node-id (annotation-node-id item))
    (define namespace-table (hash-ref table namespace (hash)))
    (when (hash-has-key? namespace-table node-id)
      (raise-arguments-error
       'make-annotation-table
       "duplicate annotation for a node and namespace"
       "namespace" namespace
       "node-id" node-id))
    (hash-set table
              namespace
              (hash-set namespace-table node-id item))))

(define (annotations-for-node annotations node-id)
  (define node-id* (canonical-node-id node-id))
  (for/list ([item (in-list annotations)]
             #:when (equal? (annotation-node-id item) node-id*))
    item))

(define (annotations-in-namespace annotations namespace)
  (for/list ([item (in-list annotations)]
             #:when (string=? (annotation-namespace item) namespace))
    item))

(define (annotation-table-ref table namespace node-id [failure-result #f])
  (define namespace-table (hash-ref table namespace #f))
  (define node-id* (canonical-node-id node-id))
  (if (and namespace-table (hash-has-key? namespace-table node-id*))
      (hash-ref namespace-table node-id*)
      failure-result))

(provide
 (contract-out
  [struct annotation
    ([namespace namespace-id?]
     [node-id stable-id?]
     [value any/c])]
  [make-annotation-table
   (-> (listof annotation?)
       (hash/c namespace-id?
               (hash/c stable-id? annotation? #:immutable #t)
               #:immutable #t))]
  [annotations-for-node
   (-> (listof annotation?) stable-id? (listof annotation?))]
  [annotations-in-namespace
   (-> (listof annotation?) namespace-id? (listof annotation?))]
  [annotation-table-ref
   (->* ((hash/c namespace-id? (hash/c stable-id? annotation?))
         namespace-id?
         stable-id?)
        (any/c)
        any/c)]))

(module+ test
  (require rackunit)

  (define type-annotation
    (annotation "gorack.types.v1" 'n1 '(type int)))
  (define verification-annotation
    (annotation "gorack.verify.v1" 'n1 '(trusted #f)))
  (define table
    (make-annotation-table
     (list type-annotation verification-annotation)))

  (check-equal? (annotation-node-id type-annotation) "n1")
  (check-equal?
   (annotation-table-ref table "gorack.types.v1" 1)
   type-annotation)
  (check-equal?
   (length
    (annotations-for-node
     (list type-annotation verification-annotation)
     'n1))
   2)
  (check-exn
   exn:fail?
   (lambda ()
     (make-annotation-table (list type-annotation type-annotation))))
  (check-exn
   exn:fail?
   (lambda ()
     (make-annotation-table
      (list (annotation "gorack.types.v1" 'n1 'first)
            (annotation "gorack.types.v1" "n1" 'second))))))
