#lang racket/base

(require racket/contract)

;; Source metadata is intentionally independent from token.Pos.  Offsets are
;; always zero-based byte offsets within a named source.

(define (stable-id? value)
  (or (exact-nonnegative-integer? value)
      (and (string? value) (positive? (string-length value)))
      (and (symbol? value)
           (positive? (string-length (symbol->string value))))))

;; Stable envelope IDs have one in-memory and wire representation.  Accepting
;; symbols and integers at API boundaries is convenient, but retaining those
;; variants makes equality and duplicate detection depend on how an ID entered
;; the process.
(define (canonical-stable-id value)
  (cond
    [(and (string? value) (positive? (string-length value))) value]
    [(and (symbol? value)
          (positive? (string-length (symbol->string value))))
     (symbol->string value)]
    [(exact-nonnegative-integer? value) (number->string value)]
    [else
     (raise-argument-error
      'canonical-stable-id
      "(or/c non-empty-string? non-empty-symbol? exact-nonnegative-integer?)"
      value)]))

(define (optional-stable-id? value)
  (or (not value) (stable-id? value)))

(define (canonical-optional-stable-id value)
  (and value (canonical-stable-id value)))

(define (optional-string? value)
  (or (not value) (string? value)))

(define (optional-path-string? value)
  (or (not value) (path-string? value)))

(define (optional-byte-count? value)
  (or (not value) (exact-nonnegative-integer? value)))

(define (line-offsets? value)
  (and (list? value)
       (andmap exact-nonnegative-integer? value)
       (let loop ([remaining value] [previous -1])
         (cond [(null? remaining) #t]
               [(<= (car remaining) previous) #f]
               [else (loop (cdr remaining) (car remaining))]))))

(struct source-file (id path content-hash byte-length line-offsets)
  #:transparent
  #:guard
  (lambda (id path content-hash byte-length line-offsets name)
    (unless (stable-id? id)
      (raise-argument-error name "stable-id?" id))
    (unless (optional-path-string? path)
      (raise-argument-error name "(or/c path-string? #f)" path))
    (unless (optional-string? content-hash)
      (raise-argument-error name "(or/c string? #f)" content-hash))
    (unless (optional-byte-count? byte-length)
      (raise-argument-error
       name "(or/c exact-nonnegative-integer? #f)" byte-length))
    (unless (line-offsets? line-offsets)
      (raise-argument-error name "strictly-increasing-line-offset-list?" line-offsets))
    (when (and (pair? line-offsets) (not (zero? (car line-offsets))))
      (raise-arguments-error
       name
       "the first line offset must be zero"
       "line-offsets" line-offsets))
    (when (and byte-length
               (ormap (lambda (offset) (> offset byte-length)) line-offsets))
      (raise-arguments-error
       name
       "line offsets must not exceed the source byte length"
       "byte-length" byte-length
       "line-offsets" line-offsets))
    (values (canonical-stable-id id) path content-hash byte-length line-offsets)))

(struct ast-position (source-id byte-offset)
  #:transparent
  #:guard
  (lambda (source-id byte-offset name)
    (unless (stable-id? source-id)
      (raise-argument-error name "stable-id?" source-id))
    (unless (exact-nonnegative-integer? byte-offset)
      (raise-argument-error name "exact-nonnegative-integer?" byte-offset))
    (values (canonical-stable-id source-id) byte-offset)))

(struct source-span (source-id start-byte end-byte)
  #:transparent
  #:guard
  (lambda (source-id start-byte end-byte name)
    (unless (stable-id? source-id)
      (raise-argument-error name "stable-id?" source-id))
    (unless (exact-nonnegative-integer? start-byte)
      (raise-argument-error name "exact-nonnegative-integer?" start-byte))
    (unless (exact-nonnegative-integer? end-byte)
      (raise-argument-error name "exact-nonnegative-integer?" end-byte))
    (when (> start-byte end-byte)
      (raise-arguments-error
       name
       "the span start must not be after its end"
       "start-byte" start-byte
       "end-byte" end-byte))
    (values (canonical-stable-id source-id) start-byte end-byte)))

(define (source-span-length span)
  (- (source-span-end-byte span) (source-span-start-byte span)))

(define (source-span-contains-byte? span source-id byte-offset)
  (and (equal? (source-span-source-id span)
               (canonical-stable-id source-id))
       (<= (source-span-start-byte span) byte-offset)
       (< byte-offset (source-span-end-byte span))))

(define (source-span-contains-span? outer inner)
  (and (equal? (source-span-source-id outer)
               (source-span-source-id inner))
       (<= (source-span-start-byte outer)
           (source-span-start-byte inner))
       (>= (source-span-end-byte outer)
           (source-span-end-byte inner))))

(define (source-span-overlaps? left right)
  (and (equal? (source-span-source-id left)
               (source-span-source-id right))
       (< (source-span-start-byte left) (source-span-end-byte right))
       (< (source-span-start-byte right) (source-span-end-byte left))))

(define (make-source-table sources)
  (for/fold ([table (hash)]) ([source (in-list sources)])
    (define id (source-file-id source))
    (when (hash-has-key? table id)
      (raise-arguments-error
       'make-source-table "duplicate source ID" "source-id" id))
    (hash-set table id source)))

(provide
 (contract-out
  [stable-id? (-> any/c boolean?)]
  [optional-stable-id? (-> any/c boolean?)]
  [canonical-stable-id (-> stable-id? string?)]
  [canonical-optional-stable-id
   (-> optional-stable-id? (or/c string? #f))]
  [struct source-file
    ([id stable-id?]
     [path optional-path-string?]
     [content-hash optional-string?]
     [byte-length optional-byte-count?]
     [line-offsets line-offsets?])]
  [struct ast-position
    ([source-id stable-id?]
     [byte-offset exact-nonnegative-integer?])]
  [struct source-span
    ([source-id stable-id?]
     [start-byte exact-nonnegative-integer?]
     [end-byte exact-nonnegative-integer?])]
  [source-span-length (-> source-span? exact-nonnegative-integer?)]
  [source-span-contains-byte?
   (-> source-span? stable-id? exact-nonnegative-integer? boolean?)]
  [source-span-contains-span? (-> source-span? source-span? boolean?)]
  [source-span-overlaps? (-> source-span? source-span? boolean?)]
  [make-source-table
   (-> (listof source-file?) (hash/c stable-id? source-file? #:immutable #t))]))

(module+ test
  (require rackunit)

  (define source (source-file 'input "input.grk" "sha256:abc" 20 '(0 8 14)))
  (define outer (source-span 'input 2 12))
  (define inner (source-span 'input 4 8))

  (check-equal? (source-file-id source) "input")
  (check-equal? (source-span-source-id outer) "input")
  (check-equal? (hash-ref (make-source-table (list source)) "input") source)
  (check-equal? (source-span-length outer) 10)
  (check-true (source-span-contains-byte? outer 'input 2))
  (check-false (source-span-contains-byte? outer 'input 12))
  (check-true (source-span-contains-span? outer inner))
  (check-true (source-span-overlaps? outer inner))
  (check-exn exn:fail? (lambda () (source-span 'input 10 2)))
  (check-exn
   exn:fail?
   (lambda () (make-source-table (list source source))))
  (check-exn
   exn:fail?
   (lambda ()
     (make-source-table
      (list (source-file 'input #f #f #f '())
            (source-file "input" #f #f #f '())))))
  ;; A trailing newline creates an empty final line whose start is exactly EOF.
  (check-equal?
   (source-file-line-offsets (source-file 'terminal #f #f 2 '(0 2)))
   '(0 2))
  (check-exn exn:fail? (lambda () (source-file 'bad #f #f 10 '(0 7 7))))
  (check-exn exn:fail? (lambda () (source-file 'bad #f #f 10 '(1 7))))
  (check-exn exn:fail? (lambda () (source-file 'bad #f #f 10 '(0 11)))))
