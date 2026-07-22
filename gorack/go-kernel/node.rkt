#lang racket/base

;; Stable, toolchain-independent storage used by the generated Go kernel.
;; Nothing in this module enumerates Go node kinds or fields.

(require racket/format
         racket/list
         racket/match
         racket/string
         (only-in "../metadata/source.rkt"
                  ast-position
                  ast-position?
                  ast-position-source-id
                  ast-position-byte-offset
                  canonical-optional-stable-id))

(provide (struct-out ir-node)
         (struct-out ir-ref)
         (struct-out ir-null)
         ir-null-value
         (struct-out ir-token)
         ast-position
         ast-position?
         ast-position-source-id
         ast-position-byte-offset
         (struct-out enum-value)
         make-ir-node
         canonical-node-id
         fresh-node-id
         reset-node-ids!
         ir-node-field
         ir-node-has-field?
         ir-node-set-field
         ir-node-update-field
         ir-node-children
         ir-node-walk
         ir-node-map
         ir-node-structural=?
         register-token!
         normalize-token-name
         token-name->spelling
         token-spelling->name
         normalize-enum-names
         ir-value->ergonomic
         ir-node-list->ergonomic
         ir-node-map->ergonomic)

;; Fields are keyed by the exact exported Go field name.  Values may be other
;; ir-nodes while an AST is being built; the wire codec replaces those values
;; with ir-refs when it creates the canonical node table.
(struct ir-node (id namespace kind fields origin-id)
  #:transparent
  #:guard
  (lambda (id namespace kind fields origin-id who)
    (unless (and (string? namespace) (not (string=? namespace "")))
      (raise-argument-error who "non-empty string?" namespace))
    (unless (and (string? kind) (not (string=? kind "")))
      (raise-argument-error who "non-empty string?" kind))
    (unless (hash? fields)
      (raise-argument-error who "hash?" fields))
    (values (canonical-node-id id)
            namespace
            kind
            (for/hash ([(key value) (in-hash fields)])
              (unless (string? key)
                (raise-arguments-error who
                                       "field names must be exact Go field-name strings"
                                       "field" key))
              (values key value))
            (canonical-optional-stable-id origin-id))))

(struct ir-ref (id)
  #:transparent
  #:guard (lambda (id who) (canonical-node-id id)))

;; A generic pass-through tool must distinguish JSON null from false and must
;; retain a symbolic token's wire tag even when its schema is newer.
(struct ir-null () #:transparent)
(define ir-null-value (ir-null))
(struct ir-token (name)
  #:transparent
  #:guard (lambda (name who) (normalize-token-name name)))

;; Enum values are symbolic and may contain more than one name for flag enums
;; such as ast.ChanDir (SEND|RECV).
(struct enum-value (type names)
  #:transparent
  #:guard
  (lambda (type names who)
    (unless (string? type)
      (raise-argument-error who "string?" type))
    (values type (normalize-enum-names names))))

(define node-id-counter (make-parameter 0))

(define (reset-node-ids!)
  (node-id-counter 0))

(define (fresh-node-id)
  (define next (add1 (node-id-counter)))
  (node-id-counter next)
  (format "n~a" next))

(define (canonical-node-id id)
  (cond
    [(and (string? id) (not (string=? id "")))
     (define numeric (regexp-match #px"^n([0-9]+)$" id))
     (when numeric
       (node-id-counter
        (max (node-id-counter) (string->number (cadr numeric)))))
     id]
    [(symbol? id) (canonical-node-id (symbol->string id))]
    [(and (exact-integer? id) (>= id 0))
     ;; Route through the string branch so explicit numeric IDs reserve the
     ;; allocator just like explicit "nN" IDs do.
     (canonical-node-id (format "n~a" id))]
    [else (raise-argument-error 'canonical-node-id
                                "(or/c non-empty-string? symbol? exact-nonnegative-integer?)"
                                id)]))

(define (fields->immutable-hash fields)
  (cond
    [(hash? fields)
     (for/hash ([(key value) (in-hash fields)])
       (values key value))]
    [(list? fields)
     (for/hash ([entry (in-list fields)])
       (match entry
         [(list (? string? key) value) (values key value)]
         [(cons (? string? key) value) (values key value)]
         [_ (raise-arguments-error 'make-ir-node
                                   "expected field association list"
                                   "entry" entry)]))]
    [else (raise-argument-error 'make-ir-node "(or/c hash? list?)" fields)]))

(define (make-ir-node id namespace kind fields [origin-id #f])
  ;; Missing IDs allocate only at this construction boundary. References,
  ;; lookups, and decoded roots must always carry an explicit ID.
  (ir-node (if id (canonical-node-id id) (fresh-node-id))
           namespace
           kind
           (fields->immutable-hash fields)
           origin-id))

(define absent (gensym 'absent))

(define (ir-node-field node field [default absent])
  (unless (ir-node? node)
    (raise-argument-error 'ir-node-field "ir-node?" node))
  (unless (string? field)
    (raise-argument-error 'ir-node-field "string?" field))
  (cond
    [(hash-has-key? (ir-node-fields node) field)
     (hash-ref (ir-node-fields node) field)]
    [(eq? default absent)
     (raise-arguments-error 'ir-node-field
                            "node has no such field"
                            "namespace" (ir-node-namespace node)
                            "kind" (ir-node-kind node)
                            "field" field)]
    [else default]))

(define (ir-node-has-field? node field)
  (and (ir-node? node)
       (string? field)
       (hash-has-key? (ir-node-fields node) field)))

(define (ir-node-set-field node field value)
  (unless (ir-node? node)
    (raise-argument-error 'ir-node-set-field "ir-node?" node))
  (unless (string? field)
    (raise-argument-error 'ir-node-set-field "string?" field))
  (struct-copy ir-node node
               [fields (hash-set (ir-node-fields node) field value)]))

(define (ir-node-update-field node field proc [default absent])
  (unless (procedure? proc)
    (raise-argument-error 'ir-node-update-field "procedure?" proc))
  (ir-node-set-field node field (proc (ir-node-field node field default))))

(define (value-children value)
  (cond
    [(ir-node? value) (list value)]
    [(list? value) (append-map value-children value)]
    [(vector? value) (append-map value-children (vector->list value))]
    [(hash? value)
     (append-map value-children
                 (for/list ([key (in-list (sort (hash-keys value) string<? #:key ~a))])
                   (hash-ref value key)))]
    [else '()]))

(define (ir-node-children node)
  (unless (ir-node? node)
    (raise-argument-error 'ir-node-children "ir-node?" node))
  (append-map value-children
              (for/list ([key (in-list (sort (hash-keys (ir-node-fields node)) string<?))])
                (hash-ref (ir-node-fields node) key))))

;; Walk each node ID once, which preserves the intended graph semantics for
;; shared comments and other shared child objects.
(define (ir-node-walk root visit)
  (unless (ir-node? root)
    (raise-argument-error 'ir-node-walk "ir-node?" root))
  (unless (procedure? visit)
    (raise-argument-error 'ir-node-walk "procedure?" visit))
  (define seen (make-hash))
  (let walk ([node root])
    (unless (hash-has-key? seen (ir-node-id node))
      (hash-set! seen (ir-node-id node) #t)
      (visit node)
      (for-each walk (ir-node-children node))))
  (void))

(define (map-value value proc memo)
  (cond
    [(ir-node? value) (map-node value proc memo)]
    [(list? value) (map (lambda (item) (map-value item proc memo)) value)]
    [(vector? value)
     (list->vector
      (map (lambda (item) (map-value item proc memo)) (vector->list value)))]
    [(hash? value)
     (for/hash ([(key item) (in-hash value)])
       (values key (map-value item proc memo)))]
    [else value]))

(define (map-node node proc memo)
  (hash-ref!
   memo
   (ir-node-id node)
   (lambda ()
     (define mapped
       (struct-copy ir-node node
                    [fields
                     (for/hash ([(key value) (in-hash (ir-node-fields node))])
                       (values key (map-value value proc memo)))]))
     (proc mapped))))

(define (ir-node-map root proc)
  (unless (ir-node? root)
    (raise-argument-error 'ir-node-map "ir-node?" root))
  (unless (procedure? proc)
    (raise-argument-error 'ir-node-map "procedure?" proc))
  (map-node root proc (make-hash)))

(define (ir-node-structural=? left right #:positions? [positions? #t])
  (define seen (make-hash))
  (define (same-value? a b)
    (cond
      [(and (ir-node? a) (ir-node? b)) (same-node? a b)]
      [(and (ast-position? a) (ast-position? b))
       (or (not positions?) (equal? a b))]
      [(and (list? a) (list? b))
       (and (= (length a) (length b)) (andmap same-value? a b))]
      [(and (vector? a) (vector? b))
       (same-value? (vector->list a) (vector->list b))]
      [(and (hash? a) (hash? b))
       (and (equal? (sort (hash-keys a) string<? #:key ~a)
                    (sort (hash-keys b) string<? #:key ~a))
            (for/and ([key (in-list (hash-keys a))])
              (same-value? (hash-ref a key) (hash-ref b key))))]
      [else (equal? a b)]))
  (define (same-node? a b)
    (define pair-key (cons (ir-node-id a) (ir-node-id b)))
    (cond
      [(hash-ref seen pair-key #f) #t]
      [else
       (hash-set! seen pair-key #t)
       (and (string=? (ir-node-namespace a) (ir-node-namespace b))
            (string=? (ir-node-kind a) (ir-node-kind b))
            (same-value? (ir-node-fields a) (ir-node-fields b)))]))
  (and (ir-node? left) (ir-node? right) (same-node? left right)))

;; Generated token modules populate these registries.  Keeping the mechanism
;; generic lets an older Racket process retain a token name it does not know.
(define token-names (make-hash))
(define token-spellings (make-hash))

(define (register-token! name spelling)
  (define name* (string-upcase (~a name)))
  (define spelling* (~a spelling))
  (hash-set! token-names name* spelling*)
  (hash-set! token-spellings spelling* name*)
  name*)

(define (token-name->spelling name [default #f])
  (hash-ref token-names (string-upcase (~a name)) default))

(define (token-spelling->name spelling [default #f])
  (hash-ref token-spellings (~a spelling) default))

(define (normalize-token-name value)
  (define text (~a value))
  (cond
    [(hash-has-key? token-names (string-upcase text)) (string-upcase text)]
    [(hash-has-key? token-spellings text) (hash-ref token-spellings text)]
    ;; Unknown symbolic names are deliberately retained for pass-through.
    [else (string-upcase text)]))

(define (normalize-enum-names value)
  (define raw
    (cond
      [(enum-value? value) (enum-value-names value)]
      [(list? value) value]
      [(vector? value) (vector->list value)]
      [else (list value)]))
  (define expanded
    (append-map
     (lambda (name)
       (define text (string-upcase (~a name)))
       (if (member text '("BOTH" "SEND|RECV"))
           '("SEND" "RECV")
           (string-split text "|")))
     raw))
  (remove-duplicates expanded string=?))

;; Canonical wire tags stay in ir-node fields so materializing references is
;; lossless. Generated accessors use these helpers to present convenient
;; values without mutating that canonical representation.
(define (ir-value->ergonomic value)
  (cond
    [(ir-null? value) #f]
    [(ir-token? value) (ir-token-name value)]
    [(enum-value? value) (enum-value-names value)]
    ;; Nodes and references are graph values, not containers to unwrap.
    [(or (ir-node? value) (ir-ref? value)) value]
    [(list? value) (map ir-value->ergonomic value)]
    [(vector? value)
     (list->vector (map ir-value->ergonomic (vector->list value)))]
    [(hash? value)
     (for/hash ([(key item) (in-hash value)])
       (values key (ir-value->ergonomic item)))]
    [else value]))

(define (ir-node-list->ergonomic value)
  (define ergonomic (ir-value->ergonomic value))
  (if ergonomic ergonomic '()))

(define (ir-node-map->ergonomic value)
  (define ergonomic (ir-value->ergonomic value))
  (if ergonomic ergonomic (hash)))

(module+ test
  (require rackunit)
  (reset-node-ids!)
  (define child (make-ir-node #f "go/ast" "Ident" '(("Name" . "x"))))
  (define root
    (make-ir-node #f "go/ast" "BinaryExpr"
                  `(("X" . ,child) ("Op" . "ADD") ("Y" . ,child))))
  (check-equal? (length (ir-node-children root)) 2)
  (define visited '())
  (ir-node-walk root (lambda (node) (set! visited (cons (ir-node-id node) visited))))
  (check-equal? (length visited) 2)
  (check-true (ir-node-structural=? root root))
  (void (register-token! "ADD" "+"))
  (check-equal? (normalize-token-name '+) "ADD")
  (check-equal? (normalize-enum-names 'BOTH) '("SEND" "RECV"))
  (reset-node-ids!)
  (void (canonical-node-id "n41"))
  (check-equal? (fresh-node-id) "n42")
  (reset-node-ids!)
  (void (canonical-node-id 5))
  (check-equal? (fresh-node-id) "n6")
  (reset-node-ids!)
  (check-equal? (ir-node-id (make-ir-node #f "go/ast" "Ident" '())) "n1")
  (check-exn exn:fail? (lambda () (canonical-node-id #f)))
  (check-exn exn:fail? (lambda () (ir-ref #f)))
  (check-false (ir-value->ergonomic ir-null-value))
  (check-equal? (ir-value->ergonomic (ir-token "ADD")) "ADD")
  (check-equal?
   (ir-value->ergonomic
    (list (ir-token "ADD")
          (vector ir-null-value)
          (hash "direction" (enum-value "go/ast.ChanDir" '(SEND)))))
   (list "ADD" (vector #f) (hash "direction" '("SEND"))))
  (check-equal? (ir-node-list->ergonomic ir-null-value) '())
  (check-equal? (ir-node-list->ergonomic #f) '())
  (check-equal? (ir-node-map->ergonomic ir-null-value) (hash))
  (check-equal? (ir-node-map->ergonomic #f) (hash))
  (check-equal?
   (ir-node-origin-id
    (make-ir-node "custom" "go/ast" "Ident" '() 'origin-1))
   "origin-1"))
