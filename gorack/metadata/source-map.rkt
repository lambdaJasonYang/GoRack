#lang racket/base

(require racket/contract
         racket/list
         "source.rkt"
         "origin.rkt"
         (only-in "../go-kernel/node.rkt" canonical-node-id))

(struct generated-range (source-id start-byte end-byte)
  #:transparent
  #:guard
  (lambda (source-id start-byte end-byte struct-name)
    (unless (stable-id? source-id)
      (raise-argument-error struct-name "stable-id?" source-id))
    (unless (exact-nonnegative-integer? start-byte)
      (raise-argument-error
       struct-name "exact-nonnegative-integer?" start-byte))
    (unless (exact-nonnegative-integer? end-byte)
      (raise-argument-error
       struct-name "exact-nonnegative-integer?" end-byte))
    (when (> start-byte end-byte)
      (raise-arguments-error
       struct-name
       "the generated range start must not be after its end"
       "start-byte" start-byte
       "end-byte" end-byte))
    (values (canonical-stable-id source-id) start-byte end-byte)))

(struct source-map-entry (generated node-id origin-id)
  #:transparent
  #:guard
  (lambda (generated node-id origin-id struct-name)
    (unless (generated-range? generated)
      (raise-argument-error struct-name "generated-range?" generated))
    (unless (stable-id? node-id)
      (raise-argument-error struct-name "stable-id?" node-id))
    (unless (optional-stable-id? origin-id)
      (raise-argument-error struct-name "(or/c stable-id? #f)" origin-id))
    (values generated
            (canonical-node-id node-id)
            (canonical-optional-stable-id origin-id))))

(struct source-map (entries)
  #:transparent
  #:guard
  (lambda (entries struct-name)
    (unless (and (list? entries) (andmap source-map-entry? entries))
      (raise-argument-error struct-name "(listof source-map-entry?)" entries))
    (unless (= (length entries) (length (remove-duplicates entries equal?)))
      (raise-arguments-error struct-name "duplicate source-map entry"))
    entries))

(struct source-map-resolution (entry original-spans) #:transparent)

(define (generated-range-length range)
  (- (generated-range-end-byte range)
     (generated-range-start-byte range)))

(define (generated-range-contains-byte? range source-id byte-offset)
  (and (equal? (generated-range-source-id range)
               (canonical-stable-id source-id))
       (<= (generated-range-start-byte range) byte-offset)
       (< byte-offset (generated-range-end-byte range))))

(define (source-map-lookup map source-id byte-offset)
  (sort
   (for/list ([entry (in-list (source-map-entries map))]
              #:when
              (generated-range-contains-byte?
               (source-map-entry-generated entry)
               source-id
               byte-offset))
     entry)
   (lambda (left right)
     (< (generated-range-length (source-map-entry-generated left))
        (generated-range-length (source-map-entry-generated right))))))

(define (source-map-entries-for-node map node-id)
  (define node-id* (canonical-node-id node-id))
  (for/list ([entry (in-list (source-map-entries map))]
             #:when (equal? (source-map-entry-node-id entry) node-id*))
    entry))

(define (source-map-entries-for-origin map origin-id)
  (define origin-id* (canonical-stable-id origin-id))
  (for/list ([entry (in-list (source-map-entries map))]
             #:when (equal? (source-map-entry-origin-id entry) origin-id*))
    entry))

(define (source-map-resolve map source-id byte-offset origin-table)
  (for/list ([entry (in-list (source-map-lookup map source-id byte-offset))])
    (source-map-resolution
     entry
     (if (source-map-entry-origin-id entry)
         (resolve-origin-source-spans
          origin-table
          (source-map-entry-origin-id entry))
         '()))))

(provide
 (contract-out
  [struct generated-range
    ([source-id stable-id?]
     [start-byte exact-nonnegative-integer?]
     [end-byte exact-nonnegative-integer?])]
  [struct source-map-entry
    ([generated generated-range?]
     [node-id stable-id?]
     [origin-id optional-stable-id?])]
  [struct source-map ([entries (listof source-map-entry?)])]
  [struct source-map-resolution
    ([entry source-map-entry?]
     [original-spans (listof source-span?)])]
  [generated-range-length
   (-> generated-range? exact-nonnegative-integer?)]
  [generated-range-contains-byte?
   (-> generated-range? stable-id? exact-nonnegative-integer? boolean?)]
  [source-map-lookup
   (-> source-map? stable-id? exact-nonnegative-integer?
       (listof source-map-entry?))]
  [source-map-entries-for-node
   (-> source-map? stable-id? (listof source-map-entry?))]
  [source-map-entries-for-origin
   (-> source-map? stable-id? (listof source-map-entry?))]
  [source-map-resolve
   (-> source-map?
       stable-id?
       exact-nonnegative-integer?
       (hash/c stable-id? origin?)
       (listof source-map-resolution?))]))

(module+ test
  (require rackunit)

  (define origins
    (make-origin-table
     (list
      (origin 'o1 'source #f #f '() (list (source-span 'input 1 6)) (hash))
      (origin 'o2 'derived 'lower 'o1 '(o1) '() (hash)))))
  (define outer
    (source-map-entry (generated-range 'output 10 30) 'n1 'o1))
  (define inner
    (source-map-entry (generated-range 'output 14 18) 'n2 'o2))
  (define map (source-map (list outer inner)))

  (check-equal? (source-map-lookup map 'output 15) (list inner outer))
  (check-equal? (source-map-entry-node-id outer) "n1")
  (check-equal? (source-map-entry-origin-id outer) "o1")
  (check-equal? (source-map-entries-for-node map 1) (list outer))
  (check-equal?
   (source-map-resolution-original-spans
    (car (source-map-resolve map 'output 15 origins)))
   (list (source-span 'input 1 6)))
  (check-exn
   exn:fail?
   (lambda () (generated-range 'output 5 2))))
