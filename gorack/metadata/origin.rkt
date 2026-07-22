#lang racket/base

(require racket/contract
         racket/list
         "source.rkt")

(define (name? value)
  (or (and (string? value) (positive? (string-length value)))
      (and (symbol? value)
           (positive? (string-length (symbol->string value))))))

(define (canonical-name value)
  (cond
    [(and (string? value) (positive? (string-length value))) value]
    [(and (symbol? value)
          (positive? (string-length (symbol->string value))))
     (symbol->string value)]
    [else
     (raise-argument-error
      'canonical-name "non-empty string or symbol" value)]))

(define (optional-name? value)
  (or (not value) (name? value)))

(struct origin (id kind pass primary inputs source-spans metadata)
  #:transparent
  #:guard
  (lambda (id kind pass primary inputs source-spans metadata struct-name)
    (unless (stable-id? id)
      (raise-argument-error struct-name "stable-id?" id))
    (unless (name? kind)
      (raise-argument-error struct-name "non-empty string or symbol" kind))
    (unless (optional-name? pass)
      (raise-argument-error
       struct-name "(or/c non-empty-string-or-symbol? #f)" pass))
    (unless (optional-stable-id? primary)
      (raise-argument-error struct-name "(or/c stable-id? #f)" primary))
    (unless (and (list? inputs) (andmap stable-id? inputs))
      (raise-argument-error struct-name "(listof stable-id?)" inputs))
    (unless (and (list? source-spans) (andmap source-span? source-spans))
      (raise-argument-error struct-name "(listof source-span?)" source-spans))
    (unless (hash? metadata)
      (raise-argument-error struct-name "hash?" metadata))
    (values (canonical-stable-id id)
            (canonical-name kind)
            (and pass (canonical-name pass))
            (canonical-optional-stable-id primary)
            (map canonical-stable-id inputs)
            source-spans
            metadata)))

(define (origin-dependencies item)
  (remove-duplicates
   (if (origin-primary item)
       (cons (origin-primary item) (origin-inputs item))
       (origin-inputs item))
   equal?))

(define (validate-origin-table table)
  (for ([(id item) (in-hash table)])
    (unless (equal? id (origin-id item))
      (raise-arguments-error
       'validate-origin-table
       "origin table key does not match the origin ID"
       "table-key" id
       "origin-id" (origin-id item)))
    (for ([dependency (in-list (origin-dependencies item))])
      (unless (hash-has-key? table dependency)
        (raise-arguments-error
         'validate-origin-table
         "origin refers to an unknown origin"
         "origin-id" id
         "unknown-origin-id" dependency))))

  ;; Origins are provenance DAGs. A cycle makes primary-source lookup
  ;; ambiguous and is rejected at the envelope boundary.
  (define visiting (make-hash))
  (define visited (make-hash))
  (define (visit id path)
    (when (hash-ref visiting id #f)
      (raise-arguments-error
       'validate-origin-table
       "origin dependency cycle"
       "cycle" (reverse (cons id path))))
    (unless (hash-ref visited id #f)
      (hash-set! visiting id #t)
      (for ([dependency
             (in-list (origin-dependencies (hash-ref table id)))])
        (visit dependency (cons id path)))
      (hash-remove! visiting id)
      (hash-set! visited id #t)))
  (for ([id (in-hash-keys table)])
    (visit id '()))
  table)

(define (make-origin-table origins)
  (define table
    (for/fold ([table (hash)]) ([item (in-list origins)])
      (define id (origin-id item))
      (when (hash-has-key? table id)
        (raise-arguments-error
         'make-origin-table "duplicate origin ID" "origin-id" id))
      (hash-set table id item)))
  (validate-origin-table table))

(define (origin-ancestors table id)
  (define id* (canonical-stable-id id))
  (unless (hash-has-key? table id*)
    (raise-arguments-error
     'origin-ancestors "unknown origin ID" "origin-id" id*))
  (define seen (make-hash))
  (define result '())
  (define (visit current)
    (unless (hash-ref seen current #f)
      (hash-set! seen current #t)
      (for ([dependency
             (in-list (origin-dependencies (hash-ref table current)))])
        (set! result (cons dependency result))
        (visit dependency))))
  (visit id*)
  (reverse (remove-duplicates result equal?)))

(define (resolve-origin-source-spans table id)
  (define id* (canonical-stable-id id))
  (unless (hash-has-key? table id*)
    (raise-arguments-error
     'resolve-origin-source-spans "unknown origin ID" "origin-id" id*))
  (define ids (cons id* (origin-ancestors table id*)))
  (remove-duplicates
   (append-map
    (lambda (origin-id)
      (origin-source-spans (hash-ref table origin-id)))
    ids)
   equal?))

(provide
 (contract-out
  [struct origin
    ([id stable-id?]
     [kind name?]
     [pass optional-name?]
     [primary optional-stable-id?]
     [inputs (listof stable-id?)]
     [source-spans (listof source-span?)]
     [metadata hash?])]
  [origin-dependencies (-> origin? (listof stable-id?))]
  [validate-origin-table
   (-> (hash/c stable-id? origin?)
       (hash/c stable-id? origin?))]
  [make-origin-table
   (-> (listof origin?) (hash/c stable-id? origin? #:immutable #t))]
  [origin-ancestors
   (-> (hash/c stable-id? origin?) stable-id? (listof stable-id?))]
  [resolve-origin-source-spans
   (-> (hash/c stable-id? origin?) stable-id? (listof source-span?))]))

(module+ test
  (require rackunit)

  (define source-origin
    (origin 'o1 'source #f #f '() (list (source-span 'input 4 9))
            (hasheq 'file "input.grk")))
  (define lowered-origin
    (origin 'o2 'derived 'lower-for 'o1 (list 'o1) '() (hash)))
  (define table (make-origin-table (list source-origin lowered-origin)))

  (check-equal? (origin-id source-origin) "o1")
  (check-equal? (origin-kind source-origin) "source")
  (check-equal? (origin-metadata source-origin)
                (hasheq 'file "input.grk"))
  (check-equal? (origin-ancestors table 'o2) '("o1"))
  (check-equal?
   (resolve-origin-source-spans table 'o2)
   (list (source-span 'input 4 9)))
  (check-exn
   exn:fail?
   (lambda ()
     (make-origin-table
      (list (origin 'bad 'derived 'pass 'missing '(missing) '() (hash))))))
  (check-exn
   exn:fail?
   (lambda ()
     (make-origin-table
      (list (origin 'a 'derived 'pass 'b '(b) '() (hash))
            (origin 'b 'derived 'pass 'a '(a) '() (hash))))))
  (check-exn
   exn:fail?
   (lambda ()
     (make-origin-table
      (list (origin 'same 'source #f #f '() '() (hash))
            (origin "same" 'source #f #f '() '() (hash)))))))
