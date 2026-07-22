#lang racket/base

(require json
         racket/format
         racket/list
         racket/match
         racket/port
         racket/string
         "node.rkt"
         "schema.rkt"
         (only-in "../metadata/namespace.rkt" extension-namespace?)
         (only-in "../metadata/source.rkt"
                  source-file
                  source-file?
                  source-file-id
                  source-file-path
                  source-file-content-hash
                  source-file-byte-length
                  source-file-line-offsets
                  stable-id?
                  canonical-stable-id
                  source-span
                  source-span?
                  source-span-source-id
                  source-span-start-byte
                  source-span-end-byte)
         (only-in "../metadata/origin.rkt"
                  origin
                  origin?
                  origin-id
                  origin-kind
                  origin-pass
                  origin-primary
                  origin-inputs
                  origin-source-spans
                  origin-metadata
                  make-origin-table)
         (only-in "../metadata/annotation.rkt"
                  annotation
                  annotation?
                  annotation-namespace
                  annotation-node-id
                  annotation-value
                  make-annotation-table))

(provide (struct-out go-ast-unit)
         make-go-ast-unit
         go-ast-unit-root-node
         go-ast-unit-node-ref
         validate-go-ast-unit
         go-ast-unit->jsexpr
         jsexpr->go-ast-unit
         write-go-ast-json
         write-go-ast-json-file
         read-go-ast-json
         read-go-ast-json-file
         go-ast-unit->sexpr
         sexpr->go-ast-unit
         write-go-ast-sexpr
         read-go-ast-sexpr
)

(define wire-format "gorack-go-ast")
(define wire-format-version 1)

;; nodes is an immutable hash from Node ID to canonical ir-node.  Canonical
;; node fields contain ir-ref values, so identity is explicit and shared
;; objects are never recursively duplicated.
(struct go-ast-unit
  (format format-version schema root nodes sources origins annotations extensions)
  #:transparent)

(define missing (gensym 'missing))


(define (jref object key [default missing])
  (define symbol-key (if (symbol? key) key (string->symbol key)))
  (define string-key (if (string? key) key (symbol->string key)))
  (cond [(and (hash? object) (hash-has-key? object symbol-key))
         (hash-ref object symbol-key)]
        [(and (hash? object) (hash-has-key? object string-key))
         (hash-ref object string-key)]
        [(eq? default missing)
         (raise-arguments-error 'jsexpr->go-ast-unit
                                "wire object is missing a required property"
                                "property" string-key)]
        [else default]))

(define (schema-identity->hash schema)
  (define identity (go-schema-identity schema))
  (hasheq 'goVersion (schema-identity-go-version identity)
          'schemaHash (schema-identity-schema-hash identity)
          'astPackageHash (schema-identity-ast-package-hash identity)
          'tokenPackageHash (schema-identity-token-package-hash identity)))

(define (wire-field-schema schema node field)
  (and (string=? (ir-node-namespace node) "go/ast")
       (go-schema-field-ref (ir-node-kind node) field schema #f)))

(define (wire-kind field-schema value)
  (if field-schema
      (go-field-schema-kind field-schema)
      (cond [(or (ir-node? value) (ir-ref? value)) 'node]
            [(ast-position? value) 'position]
            [(enum-value? value) 'enum]
            [(boolean? value) 'bool]
            [(string? value) 'string]
            [(exact-integer? value) 'int]
            [(list? value) 'list]
            [(hash? value) 'map]
            [else 'unknown])))

(define (normalize-position position source-records declared-source-sizes)
  (define source (ast-position-source-id position))
  (define offset (ast-position-byte-offset position))
  (define declared-size (hash-ref declared-source-sizes source #f))
  (when (and declared-size (> offset declared-size))
    (raise-arguments-error 'make-go-ast-unit
                           "AST position is outside its declared source"
                           "source" source
                           "offset" offset
                           "source-size" declared-size))
  (define existing
    (hash-ref source-records source
              (hasheq 'id source 'name source 'size 0)))
  (hash-set! source-records source
             (hash-set existing 'size (max (jref existing 'size 0) offset)))
  (ast-position source offset))

(define (source-file->wire source)
  (define id (source-file-id source))
  (define raw-path (source-file-path source))
  (define name (if raw-path (path->string (if (path? raw-path) raw-path (string->path raw-path))) id))
  (define base
    (hasheq 'id id
            'name name
            'size (or (source-file-byte-length source) 0)))
  (define with-hash
    (if (source-file-content-hash source)
        (hash-set base 'contentHash (source-file-content-hash source))
        base))
  (if (null? (source-file-line-offsets source))
      with-hash
      (hash-set with-hash 'lineOffsets (source-file-line-offsets source))))

(define (wire->source-file raw)
  (unless (hash? raw)
    (raise-argument-error 'jsexpr->go-ast-unit "source wire hash?" raw))
  (define id (canonical-stable-id (jref raw 'id)))
  (define name (jref raw 'name id))
  (define size (jref raw 'size 0))
  (source-file id name (jref raw 'contentHash #f) size
               (jref raw 'lineOffsets '())))

(define (source-record->wire source)
  (cond
    [(source-file? source) (source-file->wire source)]
    [(hash? source) (source-file->wire (wire->source-file source))]
    [else
     (raise-argument-error
      'validate-go-ast-unit "(or/c source-file? hash?)" source)]))

(define (make-go-ast-unit root
                          #:schema [schema #f]
                          #:sources [declared-sources '()]
                          #:origins [origins '()]
                          #:annotations [annotations '()]
                          #:extensions [extensions '()]
                          #:allow-unknown? [allow-unknown? #f])
  (unless (ir-node? root)
    (raise-argument-error 'make-go-ast-unit "ir-node?" root))
  (define schema* (or schema (current-go-schema) (load-go-schema)))
  (current-go-schema schema*)
  (define table (make-hash))
  (define originals (make-hash))
  (define source-records (make-hash))
  (define declared-source-sizes (make-hash))
  (for ([source (in-list declared-sources)])
    (unless (source-file? source)
      (raise-argument-error 'make-go-ast-unit "(listof source-file?)" declared-sources))
    (define wire-source (source-file->wire source))
    (define id (jref wire-source 'id))
    (when (hash-has-key? source-records id)
      (raise-arguments-error 'make-go-ast-unit
                             "duplicate declared source ID"
                             "source" id))
    (hash-set! source-records id wire-source)
    (when (source-file-byte-length source)
      (hash-set! declared-source-sizes id (source-file-byte-length source))))

  (define (canonical-value owner field value)
    (define field-schema (wire-field-schema schema* owner field))
    (define kind (wire-kind field-schema value))
    (cond
      [(ir-null? value) value]
      [(ir-token? value) value]
      [(not value)
       (if (and field-schema (not (eq? kind 'bool))) ir-null-value #f)]
      [(ir-node? value)
       (visit value)
       (ir-ref (ir-node-id value))]
      [(ir-ref? value) value]
      [(ast-position? value)
       (normalize-position value source-records declared-source-sizes)]
      [(enum-value? value)
       (enum-value (enum-value-type value) (enum-value-names value))]
      [(eq? kind 'token) (ir-token value)]
      [(and (eq? kind 'enum) value)
       (enum-value (or (and field-schema (go-field-schema-enum-type field-schema)) "")
                   (normalize-enum-names value))]
      [(list? value)
       (for/list ([item (in-list value)])
         (canonical-value owner field item))]
      [(vector? value)
       (for/list ([item (in-vector value)])
         (canonical-value owner field item))]
      [(hash? value)
       (for/hash ([(key item) (in-hash value)])
         (values (~a key) (canonical-value owner field item)))]
      [else value]))

  (define (visit node)
    (define id (ir-node-id node))
    (when (string=? (ir-node-namespace node) "go/ast")
      (validate-ir-node node #:schema schema* #:allow-unknown? allow-unknown?))
    (define existing (hash-ref originals id #f))
    (when (and existing (not (eq? existing node)) (not (equal? existing node)))
      (raise-arguments-error 'make-go-ast-unit
                             "two different nodes use the same node ID"
                             "id" id
                             "first-kind" (ir-node-kind existing)
                             "second-kind" (ir-node-kind node)))
    (unless existing
      (hash-set! originals id node)
      ;; Mark before descending so a deliberately cyclic extension graph does
      ;; not recurse forever. The placeholder is replaced after its fields.
      (hash-set! table id #f)
      (define fields
        (for/hash ([(field value) (in-hash (ir-node-fields node))])
          (values field (canonical-value node field value))))
      (hash-set! table id (struct-copy ir-node node [fields fields]))))

  (visit root)
  (define sources
    (for/list ([source (in-list (sort (hash-keys source-records) string<?))])
      (wire->source-file (hash-ref source-records source))))
  (define unit
    (go-ast-unit wire-format
                 wire-format-version
                 (schema-identity->hash schema*)
                 (ir-node-id root)
                 (for/hash ([(id node) (in-hash table)]) (values id node))
                 sources
                 origins
                 annotations
                 extensions))
  (validate-go-ast-unit unit
                        #:schema schema*
                        #:allow-schema-mismatch? #f
                        #:allow-unknown? allow-unknown?)
  unit)

(define (go-ast-unit-node-ref unit id [default missing])
  (define canonical (canonical-node-id id))
  (define found (hash-ref (go-ast-unit-nodes unit) canonical missing))
  (cond [(not (eq? found missing)) found]
        [(not (eq? default missing)) default]
        [else (raise-arguments-error 'go-ast-unit-node-ref
                                     "unknown node ID"
                                     "id" canonical)]))

(define (materialize-value unit value memo active)
  (cond
    ;; Preserve canonical tags. Generated accessors expose ergonomic views,
    ;; while direct re-emission remains byte-for-byte faithful for fields an
    ;; older generic tool does not understand.
    [(or (ir-null? value) (ir-token? value) (enum-value? value)) value]
    [(ir-ref? value)
     (define id (ir-ref-id value))
     (if (hash-ref active id #f)
         ;; Official syntax nodes are acyclic after object-resolution fields
         ;; are excluded. Retain a reference if an extension introduces one.
         value
         (materialize-node unit id memo active))]
    [(list? value)
     (map (lambda (item) (materialize-value unit item memo active)) value)]
    [(hash? value)
     (for/hash ([(key item) (in-hash value)])
       (values key (materialize-value unit item memo active)))]
    [else value]))

(define (materialize-node unit id memo active)
  (hash-ref!
   memo id
   (lambda ()
     (define canonical (go-ast-unit-node-ref unit id))
     (hash-set! active id #t)
     (define materialized
       (struct-copy
        ir-node canonical
        [fields
         (for/hash ([(field value) (in-hash (ir-node-fields canonical))])
           (values field (materialize-value unit value memo active)))]))
     (hash-remove! active id)
     materialized)))

(define (go-ast-unit-root-node unit #:materialize? [materialize? #t])
  (unless (go-ast-unit? unit)
    (raise-argument-error 'go-ast-unit-root-node "go-ast-unit?" unit))
  (if materialize?
      (materialize-node unit (go-ast-unit-root unit) (make-hash) (make-hash))
      (go-ast-unit-node-ref unit (go-ast-unit-root unit))))

(define (source-index unit)
  (for/fold ([table (hash)]) ([source (in-list (go-ast-unit-sources unit))])
    (define wire-source (source-record->wire source))
    (define id (jref wire-source 'id))
    (when (hash-has-key? table id)
      (raise-arguments-error 'validate-go-ast-unit
                             "duplicate source ID"
                             "source" id))
    (define size (jref wire-source 'size))
    (unless (exact-nonnegative-integer? size)
      (raise-arguments-error 'validate-go-ast-unit
                             "source size must be a nonnegative integer"
                             "source" id
                             "size" size))
    (hash-set table id wire-source)))

(define (value-refs value)
  (cond [(ir-ref? value) (list (ir-ref-id value))]
        [(list? value) (append-map value-refs value)]
        [(hash? value) (append-map value-refs (hash-values value))]
        [else '()]))

(define (validate-go-ast-unit unit
                              #:schema [schema #f]
                              #:allow-schema-mismatch? [allow-mismatch? #t]
                              #:allow-unknown? [allow-unknown? #f])
  (unless (go-ast-unit? unit)
    (raise-argument-error 'validate-go-ast-unit "go-ast-unit?" unit))
  (unless (string=? (go-ast-unit-format unit) wire-format)
    (raise-arguments-error 'validate-go-ast-unit
                           "unsupported wire format"
                           "format" (go-ast-unit-format unit)))
  (unless (= (go-ast-unit-format-version unit) wire-format-version)
    (raise-arguments-error 'validate-go-ast-unit
                           "unsupported wire-format version"
                           "version" (go-ast-unit-format-version unit)))
  (unless (hash-has-key? (go-ast-unit-nodes unit) (go-ast-unit-root unit))
    (raise-arguments-error 'validate-go-ast-unit
                           "root is missing from the node table"
                           "root" (go-ast-unit-root unit)))
  (define schema* (or schema (current-go-schema)))
  (define expected-schema-hash
    (and schema* (schema-identity-schema-hash (go-schema-identity schema*))))
  (define actual-schema-hash (jref (go-ast-unit-schema unit) 'schemaHash ""))
  (define schema-matches?
    (and expected-schema-hash
         (string=? expected-schema-hash actual-schema-hash)))
  (when (and schema* (not allow-mismatch?))
    (unless schema-matches?
      (raise-arguments-error 'validate-go-ast-unit
                             "wire unit was produced for a different Go AST schema"
                             "expected" expected-schema-hash
                             "actual" actual-schema-hash)))
  (define sources (source-index unit))
  (define materialized-memo (make-hash))
  (define materialized-active (make-hash))
  (for ([(id node) (in-hash (go-ast-unit-nodes unit))])
    (unless (string=? id (ir-node-id node))
      (raise-arguments-error 'validate-go-ast-unit
                             "node table key and node ID differ"
                             "table-key" id
                             "node-id" (ir-node-id node)))
    (for ([ref (in-list (append-map value-refs (hash-values (ir-node-fields node))))])
      (unless (hash-has-key? (go-ast-unit-nodes unit) ref)
        (raise-arguments-error 'validate-go-ast-unit
                               "field refers to a missing node"
                               "node" id
                               "reference" ref)))
    (for ([value (in-list (hash-values (ir-node-fields node)))])
      (define (check-position nested)
        (cond
          [(ast-position? nested)
           (define source (~a (ast-position-source-id nested)))
           (define offset (ast-position-byte-offset nested))
           (define source-record (hash-ref sources source #f))
           (unless source-record
             (raise-arguments-error 'validate-go-ast-unit
                                    "position refers to a missing source"
                                    "node" id
                                    "source" source))
           (unless (<= 0 offset (jref source-record 'size))
             (raise-arguments-error 'validate-go-ast-unit
                                    "position is outside its source"
                                    "node" id
                                    "offset" offset))]
          [(list? nested) (for-each check-position nested)]
          [(hash? nested) (for-each check-position (hash-values nested))]))
      (check-position value))
    (when (and schema-matches? (string=? (ir-node-namespace node) "go/ast"))
      (validate-ir-node
       (materialize-node unit id materialized-memo materialized-active)
       #:schema schema*
       #:allow-unknown? allow-unknown?)))

  ;; Validate the hand-written provenance envelope when it is represented by
  ;; its typed metadata structs. Decoded generic hashes remain pass-through
  ;; data and are checked by the Go-side envelope validator.
  (define typed-origins
    (filter origin? (go-ast-unit-origins unit)))
  (define origin-ids (make-hash))
  (for ([item (in-list (go-ast-unit-origins unit))])
    (define id
      (canonical-stable-id
       (if (origin? item) (origin-id item) (jref item 'id))))
    (when (hash-has-key? origin-ids id)
      (raise-arguments-error 'validate-go-ast-unit
                             "duplicate origin ID"
                             "origin" id))
    (hash-set! origin-ids id #t))
  (when (pair? typed-origins)
    (unless (= (length typed-origins) (length (go-ast-unit-origins unit)))
      (raise-arguments-error 'validate-go-ast-unit
                             "origins must be uniformly typed or wire hashes"))
    (make-origin-table typed-origins)
    (for ([item (in-list typed-origins)])
      (for ([span (in-list (origin-source-spans item))])
        (define source-id (source-span-source-id span))
        (define source-record (hash-ref sources source-id #f))
        (unless source-record
          (raise-arguments-error 'validate-go-ast-unit
                                 "origin span refers to a missing source"
                                 "origin" (origin-id item)
                                 "source" (source-span-source-id span)))
        (when (and source-record
                   (> (source-span-end-byte span) (jref source-record 'size)))
          (raise-arguments-error 'validate-go-ast-unit
                                 "origin span is outside its source"
                                 "origin" (origin-id item)
                                 "source" source-id
                                 "end-byte" (source-span-end-byte span)
                                 "source-size" (jref source-record 'size)))))
    )
  (for ([(id node) (in-hash (go-ast-unit-nodes unit))]
        #:when (ir-node-origin-id node))
    (unless (hash-has-key? origin-ids (ir-node-origin-id node))
      (raise-arguments-error 'validate-go-ast-unit
                             "node refers to a missing origin"
                             "node" id
                             "origin" (ir-node-origin-id node))))

  (define typed-annotations
    (filter annotation? (go-ast-unit-annotations unit)))
  (when (pair? typed-annotations)
    (unless (= (length typed-annotations) (length (go-ast-unit-annotations unit)))
      (raise-arguments-error 'validate-go-ast-unit
                             "annotations must be uniformly typed or wire hashes"))
    (make-annotation-table typed-annotations)
    (for ([item (in-list typed-annotations)])
      (define annotated-id (canonical-node-id (annotation-node-id item)))
      (unless (hash-has-key? (go-ast-unit-nodes unit) annotated-id)
        (raise-arguments-error 'validate-go-ast-unit
                               "annotation refers to a missing node"
                               "namespace" (annotation-namespace item)
                               "node" annotated-id))))

  (unless (list? (go-ast-unit-extensions unit))
    (raise-argument-error 'validate-go-ast-unit "list?" (go-ast-unit-extensions unit)))
  (for ([item (in-list (go-ast-unit-extensions unit))]
        [index (in-naturals)])
    (unless (hash? item)
      (raise-arguments-error 'validate-go-ast-unit
                             "extension entry must be a wire hash"
                             "index" index
                             "value" item))
    (define namespace (jref item 'namespace #f))
    (define version (jref item 'version #f))
    (define value (jref item 'value))
    (unless (extension-namespace? namespace)
      (raise-arguments-error 'validate-go-ast-unit
                             "extension namespace is invalid"
                             "index" index
                             "namespace" namespace))
    (unless (exact-positive-integer? version)
      (raise-arguments-error 'validate-go-ast-unit
                             "extension version must be a positive integer"
                             "namespace" namespace
                             "version" version))
    ;; Conversion is also the JSON-compatibility check used during emission.
    (metadata->jsexpr value))
  unit)

(define (field->jsexpr field-schema value)
  (define kind (and field-schema (go-field-schema-kind field-schema)))
  (cond
    [(ir-null? value) (json-null)]
    [(ir-token? value) (hasheq '$token (ir-token-name value))]
    ;; Builder values are canonicalized before reaching this point. An unknown
    ;; field's #f therefore means JSON false, not an inferred null.
    [(not value) #f]
    [(ir-ref? value) (hasheq '$ref (ir-ref-id value))]
    [(ast-position? value)
     (hasheq '$position
             (hasheq 'source (~a (ast-position-source-id value))
                     'byteOffset (ast-position-byte-offset value)))]
    [(eq? kind 'token)
     (hasheq '$token (normalize-token-name value))]
    [(or (eq? kind 'enum) (enum-value? value))
     (define enum
       (if (enum-value? value)
           value
           (enum-value (or (and field-schema (go-field-schema-enum-type field-schema)) "")
                       value)))
     (hasheq '$enum
             (hasheq 'type (enum-value-type enum)
                     'names (enum-value-names enum)))]
    [(list? value)
     (for/list ([item (in-list value)]) (field->jsexpr field-schema item))]
    [(hash? value)
     (for/hasheq ([(key item) (in-hash value)])
       (values (string->symbol (~a key)) (field->jsexpr field-schema item)))]
    [(symbol? value) (symbol->string value)]
    [(or (string? value) (boolean? value) (number? value)) value]
    [else (raise-arguments-error 'go-ast-unit->jsexpr
                                 "field value cannot be represented on the wire"
                                 "value" value)]))

(define (node->wire-jsexpr node schema)
  (define fields
    (for/hasheq ([(field value) (in-hash (ir-node-fields node))])
      (values (string->symbol field)
              (field->jsexpr (wire-field-schema schema node field) value))))
  (define base
    (hasheq 'id (ir-node-id node)
            'namespace (ir-node-namespace node)
            'kind (ir-node-kind node)
            'fields fields))
  (if (ir-node-origin-id node)
      (hash-set base 'origin (~a (ir-node-origin-id node)))
      base))

(define (node-id<? left right)
  (define left-match (regexp-match #px"^n([0-9]+)$" left))
  (define right-match (regexp-match #px"^n([0-9]+)$" right))
  (cond [(and left-match right-match)
         (< (string->number (cadr left-match))
            (string->number (cadr right-match)))]
        [(and left-match (not right-match)) #t]
        [(and right-match (not left-match)) #f]
        [else (string<? left right)]))

(define (metadata->jsexpr value)
  (cond
    [(source-file? value) (metadata->jsexpr (source-file->wire value))]
    [(source-span? value)
     (hasheq 'source (source-span-source-id value)
             'startByte (source-span-start-byte value)
             'endByte (source-span-end-byte value))]
    [(origin? value)
     (define base
       (hasheq 'id (origin-id value)
               'kind (origin-kind value)
               'inputs (origin-inputs value)
               'sourceSpans (map metadata->jsexpr (origin-source-spans value))))
     (define with-pass
       (if (origin-pass value)
           (hash-set base 'pass (origin-pass value))
           base))
     (define with-primary
       (if (origin-primary value)
           (hash-set with-pass 'primary (origin-primary value))
           with-pass))
     (if (zero? (hash-count (origin-metadata value)))
         with-primary
         (hash-set with-primary 'metadata
                   (metadata->jsexpr (origin-metadata value))))]
    [(annotation? value)
     (hasheq 'namespace (annotation-namespace value)
             'node (canonical-node-id (annotation-node-id value))
             'value (metadata->jsexpr (annotation-value value)))]
    [(hash? value)
     (for/hasheq ([(key item) (in-hash value)])
       (values (if (symbol? key) key (string->symbol (~a key)))
               (metadata->jsexpr item)))]
    [(list? value) (map metadata->jsexpr value)]
    [(vector? value) (map metadata->jsexpr (vector->list value))]
    [(symbol? value) (symbol->string value)]
    [(or (string? value) (boolean? value) (number? value) (eq? value (json-null))) value]
    [else (raise-arguments-error 'go-ast-unit->jsexpr
                                 "metadata value is not JSON-compatible"
                                 "value" value)]))

(define (go-ast-unit->jsexpr unit #:schema [schema #f])
  (define schema* (or schema (current-go-schema) (load-go-schema)))
  (define nodes
    (for/list ([id (in-list (sort (hash-keys (go-ast-unit-nodes unit)) node-id<?))])
      (node->wire-jsexpr (hash-ref (go-ast-unit-nodes unit) id) schema*)))
  (define base
    (hasheq 'format (go-ast-unit-format unit)
            'formatVersion (go-ast-unit-format-version unit)
            'goSchema (metadata->jsexpr (go-ast-unit-schema unit))
            'root (go-ast-unit-root unit)
            'nodes nodes
            'sources (metadata->jsexpr (go-ast-unit-sources unit))))
  (define with-origins
    (if (null? (go-ast-unit-origins unit)) base
        (hash-set base 'origins (metadata->jsexpr (go-ast-unit-origins unit)))))
  (define with-annotations
    (if (null? (go-ast-unit-annotations unit)) with-origins
        (hash-set with-origins 'annotations
                  (metadata->jsexpr (go-ast-unit-annotations unit)))))
  (if (null? (go-ast-unit-extensions unit)) with-annotations
      (hash-set with-annotations 'extensions
                (metadata->jsexpr (go-ast-unit-extensions unit)))))

(define (tagged-value->field raw field-schema)
  (cond
    [(eq? raw (json-null)) ir-null-value]
    [(list? raw)
     (map (lambda (item) (tagged-value->field item field-schema)) raw)]
    [(hash? raw)
     (define field-kind (and field-schema (go-field-schema-kind field-schema)))
     (define (single-tag? key predicate)
       (and (not (eq? field-kind 'node-map))
            (= (hash-count raw) 1)
            (or (hash-has-key? raw key)
                (hash-has-key? raw (symbol->string key)))
            (predicate (jref raw key))))
     (cond
       [(single-tag? '$ref stable-id?)
        (ir-ref (jref raw '$ref))]
       [(single-tag? '$position hash?)
        (define position (jref raw '$position))
        (ast-position (jref position 'source) (jref position 'byteOffset))]
       [(single-tag? '$token (lambda (value) (or (string? value) (symbol? value))))
        (ir-token (jref raw '$token))]
       [(single-tag? '$enum hash?)
        (define enum (jref raw '$enum))
        (enum-value (jref enum 'type) (jref enum 'names '()))]
       [else
        (for/hash ([(key value) (in-hash raw)])
          ;; A node-map's values are themselves $ref objects; do not carry the
          ;; outer map category into their tag recognition.
          (values (~a key) (tagged-value->field value #f)))])]
    [(and field-schema (eq? (go-field-schema-kind field-schema) 'token))
     (ir-token raw)]
    [else raw]))

(define (jsexpr->origin raw)
  (origin (jref raw 'id)
          (jref raw 'kind)
          (jref raw 'pass #f)
          (jref raw 'primary #f)
          (jref raw 'inputs '())
          (for/list ([span (in-list (jref raw 'sourceSpans '()))])
            (source-span (jref span 'source)
                         (jref span 'startByte)
                         (jref span 'endByte)))
          (jref raw 'metadata (hash))))

(define (jsexpr->annotation raw)
  (annotation (jref raw 'namespace)
              (jref raw 'node)
              (jref raw 'value)))

(define (jsexpr->go-ast-unit raw
                             #:schema [schema #f]
                             #:allow-schema-mismatch? [allow-mismatch? #t]
                             #:allow-unknown? [allow-unknown? #f])
  (define schema* (or schema (current-go-schema) (load-go-schema)))
  (current-go-schema schema*)
  (define nodes
    (for/fold ([table (hash)]) ([raw-node (in-list (jref raw 'nodes '()))])
      (define id (canonical-node-id (jref raw-node 'id)))
      (when (hash-has-key? table id)
        (raise-arguments-error 'jsexpr->go-ast-unit
                               "wire node table contains a duplicate ID"
                               "id" id))
      (define namespace (jref raw-node 'namespace))
      (define kind (jref raw-node 'kind))
      (define raw-fields (jref raw-node 'fields #hasheq()))
      (define node-placeholder (make-ir-node id namespace kind '()))
      (define fields
        (for/hash ([(raw-name raw-value) (in-hash raw-fields)])
          (define name (~a raw-name))
          (define field-schema (wire-field-schema schema* node-placeholder name))
          (values name (tagged-value->field raw-value field-schema))))
      (define node
        (make-ir-node id namespace kind fields (jref raw-node 'origin #f)))
      (hash-set table id node)))
  (define unit
    (go-ast-unit (jref raw 'format)
                 (jref raw 'formatVersion)
                 (jref raw 'goSchema)
                 (canonical-node-id (jref raw 'root))
                 nodes
                 (map wire->source-file (jref raw 'sources '()))
                 (map jsexpr->origin (jref raw 'origins '()))
                 (map jsexpr->annotation (jref raw 'annotations '()))
                 (jref raw 'extensions '())))
  (validate-go-ast-unit unit
                        #:schema schema*
                        #:allow-schema-mismatch? allow-mismatch?
                        #:allow-unknown? allow-unknown?))

(define (write-go-ast-json output value #:schema [schema #f])
  (define unit (if (go-ast-unit? value) value (make-go-ast-unit value #:schema schema)))
  (write-json (go-ast-unit->jsexpr unit #:schema schema) output)
  (newline output))

(define (write-go-ast-json-file path value #:schema [schema #f])
  (call-with-output-file path
    #:exists 'replace
    (lambda (output) (write-go-ast-json output value #:schema schema))))

(define (read-go-ast-json [input (current-input-port)]
                          #:schema [schema #f]
                          #:allow-schema-mismatch? [allow-mismatch? #t]
                          #:allow-unknown? [allow-unknown? #f])
  (jsexpr->go-ast-unit (read-json input)
                       #:schema schema
                       #:allow-schema-mismatch? allow-mismatch?
                       #:allow-unknown? allow-unknown?))

(define (read-go-ast-json-file path
                               #:schema [schema #f]
                               #:allow-schema-mismatch? [allow-mismatch? #t]
                               #:allow-unknown? [allow-unknown? #f])
  (call-with-input-file path
    (lambda (input)
      (read-go-ast-json input
                        #:schema schema
                        #:allow-schema-mismatch? allow-mismatch?
                        #:allow-unknown? allow-unknown?))))

(define (wire-value->sexpr value)
  (cond [(ir-null? value) '(null)]
        [(ir-token? value) `(token ,(ir-token-name value))]
        [(ir-ref? value) `(ref ,(string->symbol (ir-ref-id value)))]
        [(ast-position? value)
         `(position ,(ast-position-source-id value) ,(ast-position-byte-offset value))]
        [(enum-value? value)
         `(enum ,(enum-value-type value) ,@(enum-value-names value))]
        [(list? value) (map wire-value->sexpr value)]
        [(hash? value)
         `(map ,@(for/list ([key (in-list (sort (hash-keys value) string<? #:key ~a))])
                    (list key (wire-value->sexpr (hash-ref value key)))))]
        [(symbol? value) `(symbol ,(symbol->string value))]
        [else value]))

(define (go-ast-unit->sexpr unit)
  `(go-ast-unit
    (format ,(go-ast-unit-format unit) ,(go-ast-unit-format-version unit))
    (go-schema ,@(for/list ([key (in-list (sort (hash-keys (go-ast-unit-schema unit))
                                                string<? #:key ~a))])
                   (list key (hash-ref (go-ast-unit-schema unit) key))))
    (root ,(string->symbol (go-ast-unit-root unit)))
    (nodes
     ,@(for/list ([id (in-list (sort (hash-keys (go-ast-unit-nodes unit)) string<?))])
         (define node (hash-ref (go-ast-unit-nodes unit) id))
         `(node ,(string->symbol id)
                (namespace ,(ir-node-namespace node))
                (kind ,(ir-node-kind node))
                (fields
                 ,@(for/list ([field (in-list (sort (hash-keys (ir-node-fields node)) string<?))])
                     (list field (wire-value->sexpr
                                  (hash-ref (ir-node-fields node) field)))))
                (origin ,(ir-node-origin-id node)))))
    (sources ,@(map metadata->jsexpr (go-ast-unit-sources unit)))
    (origins ,@(map metadata->jsexpr (go-ast-unit-origins unit)))
    (annotations ,@(map metadata->jsexpr (go-ast-unit-annotations unit)))
    (extensions ,@(map metadata->jsexpr (go-ast-unit-extensions unit)))))

(define (sexpr-value->wire value)
  (match value
    [`(null) ir-null-value]
    [`(ref ,id) (ir-ref id)]
    [`(position ,source ,offset) (ast-position source offset)]
    [`(token ,name) (ir-token name)]
    [`(symbol ,name) (string->symbol name)]
    [`(enum ,type ,names ...) (enum-value type names)]
    [`(map ,entries ...)
     (for/hash ([entry (in-list entries)])
       (match entry
         [(list key item) (values (~a key) (sexpr-value->wire item))]
         [_ (raise-arguments-error 'sexpr->go-ast-unit
                                   "invalid map entry"
                                   "entry" entry)]))]
    [(? list? items) (map sexpr-value->wire items)]
    [other other]))

(define (sexpr->go-ast-unit sexpr
                            #:schema [schema #f]
                            #:allow-unknown? [allow-unknown? #f])
  (match sexpr
    [`(go-ast-unit
       (format ,format ,version)
       (go-schema ,schema-pairs ...)
       (root ,root)
       (nodes ,raw-nodes ...)
       (sources ,sources ...)
       (origins ,origins ...)
       (annotations ,annotations ...)
       (extensions ,extensions ...))
     (define schema-hash
       (for/hash ([pair (in-list schema-pairs)])
         (match pair [(list key value) (values key value)])))
     (define nodes
       (for/hash ([raw-node (in-list raw-nodes)])
         (match raw-node
           [`(node ,id
                   (namespace ,namespace)
                   (kind ,kind)
                   (fields ,field-pairs ...)
                   (origin ,origin))
            (define fields
              (for/hash ([pair (in-list field-pairs)])
                (match pair
                  [(list field value)
                   (values field (sexpr-value->wire value))])))
            (define node (make-ir-node id namespace kind fields origin))
            (values (ir-node-id node) node)])))
     (validate-go-ast-unit
      (go-ast-unit format version schema-hash (canonical-node-id root)
                   nodes (map wire->source-file sources)
                   (map jsexpr->origin origins)
                   (map jsexpr->annotation annotations)
                   extensions)
      #:schema schema
      #:allow-schema-mismatch? #t
      #:allow-unknown? allow-unknown?)]
    [_ (raise-arguments-error 'sexpr->go-ast-unit
                              "not a gorack go-ast-unit S-expression"
                              "value" sexpr)]))

(define (write-go-ast-sexpr output value #:schema [schema #f])
  (write (go-ast-unit->sexpr
          (if (go-ast-unit? value) value (make-go-ast-unit value #:schema schema)))
         output)
  (newline output))

(define (read-go-ast-sexpr [input (current-input-port)]
                           #:schema [schema #f]
                           #:allow-unknown? [allow-unknown? #f])
  (sexpr->go-ast-unit (read input)
                      #:schema schema
                      #:allow-unknown? allow-unknown?))


(module+ test
  (require rackunit)
  ;; This test uses a deliberately tiny in-memory schema so it does not depend
  ;; on generated files merely to exercise graph identity.
  (define fake-schema
    (go-schema "gorack-go-ast-schema" 1
               (schema-identity "test" "sha256:test" "sha256:ast" "sha256:token")
               '() (hasheq) '() (hasheq) '() '() (hasheq)))
  (define shared (make-ir-node "n2" "go/ast" "FutureNode" '(("Name" . "x"))))
  (define root
    (make-ir-node "n1" "go/ast" "FutureRoot"
                  `(("Left" . ,shared) ("Right" . ,shared))))
  (define unit (make-go-ast-unit root #:schema fake-schema #:allow-unknown? #t))
  (check-equal? (hash-count (go-ast-unit-nodes unit)) 2)
  (define decoded
    (jsexpr->go-ast-unit (go-ast-unit->jsexpr unit #:schema fake-schema)
                         #:schema fake-schema
                         #:allow-unknown? #t))
  (check-equal? (hash-count (go-ast-unit-nodes decoded)) 2)
  (check-equal? (go-ast-unit-root decoded) "n1")

  (define provenance-root
    (struct-copy ir-node root [origin-id 'o1]))
  (define provenance-unit
    (make-go-ast-unit
     provenance-root
     #:schema fake-schema
     #:sources (list (source-file 'input "input.grk" "sha256:input" 12 '(0 6)))
     #:origins (list (origin 'o1 'source #f #f '()
                             (list (source-span 'input 1 4))
                             (hasheq 'frontend "gorack"
                                     'revision 3)))
     #:annotations (list (annotation "gorack.types.v1" 'n1 '(type int)))
     #:allow-unknown? #t))
  (define provenance-json
    (go-ast-unit->jsexpr provenance-unit #:schema fake-schema))
  (check-equal? (jref (car (jref provenance-json 'origins)) 'id) "o1")
  (check-equal?
   (jref (jref (car (jref provenance-json 'origins)) 'metadata) 'frontend)
   "gorack")
  (check-equal? (jref (car (jref provenance-json 'annotations)) 'node) "n1")
  (define provenance-json-roundtrip
    (jsexpr->go-ast-unit provenance-json
                         #:schema fake-schema
                         #:allow-unknown? #t))
  (check-true (source-file? (car (go-ast-unit-sources provenance-json-roundtrip))))
  (check-equal?
   (source-file-id (car (go-ast-unit-sources provenance-json-roundtrip)))
   "input")
  (check-equal?
   (source-file-line-offsets
    (car (go-ast-unit-sources provenance-json-roundtrip)))
   '(0 6))
  (check-true (origin? (car (go-ast-unit-origins provenance-json-roundtrip))))
  (check-equal?
   (origin-metadata (car (go-ast-unit-origins provenance-json-roundtrip)))
   (hasheq 'frontend "gorack" 'revision 3))
  (check-true
   (annotation? (car (go-ast-unit-annotations provenance-json-roundtrip))))
  (define provenance-sexpr-roundtrip
    (sexpr->go-ast-unit (go-ast-unit->sexpr provenance-unit)
                        #:schema fake-schema
                        #:allow-unknown? #t))
  (check-true (origin? (car (go-ast-unit-origins provenance-sexpr-roundtrip))))
  (check-true (annotation? (car (go-ast-unit-annotations provenance-sexpr-roundtrip))))
  (check-true
   (source-file? (car (go-ast-unit-sources provenance-sexpr-roundtrip))))
  (check-equal?
   (origin-metadata (car (go-ast-unit-origins provenance-sexpr-roundtrip)))
   (hasheq 'frontend "gorack" 'revision 3))

  (define future
    (make-ir-node "n9" "go/ast" "FutureNode"
                  `(("False" . #f)
                    ("Null" . ,ir-null-value)
                    ("Token" . ,(ir-token "ADD")))))
  (define future-unit
    (make-go-ast-unit future #:schema fake-schema #:allow-unknown? #t))
  (define future-json (go-ast-unit->jsexpr future-unit #:schema fake-schema))
  (define future-fields (jref (car (jref future-json 'nodes)) 'fields))
  (check-false (jref future-fields 'False))
  (check-equal? (jref future-fields 'Null) (json-null))
  (check-equal? (jref (jref future-fields 'Token) '$token) "ADD")
  (define future-roundtrip
    (jsexpr->go-ast-unit future-json
                         #:schema fake-schema
                         #:allow-unknown? #t))
  (define future-json-again
    (go-ast-unit->jsexpr future-roundtrip #:schema fake-schema))
  (check-equal? future-json-again future-json)
  (define future-materialized
    (go-ast-unit-root-node future-roundtrip))
  (check-equal?
   (go-ast-unit->jsexpr
    (make-go-ast-unit future-materialized
                      #:schema fake-schema
                      #:allow-unknown? #t)
    #:schema fake-schema)
   future-json)

  (define tag-key-json
    (hash-set
     future-json
     'nodes
     (list
      (hasheq 'id "n9" 'namespace "go/ast" 'kind "FutureNode"
              'fields
              (hasheq 'Map
                      (hasheq '$ref (hasheq '$ref "n10"))))
      (hasheq 'id "n10" 'namespace "go/ast" 'kind "FutureLeaf"
              'fields (hasheq)))))
  (define tag-key-unit
    (jsexpr->go-ast-unit tag-key-json
                         #:schema fake-schema
                         #:allow-unknown? #t))
  (check-true
   (ir-ref?
    (hash-ref
     (ir-node-field (go-ast-unit-node-ref tag-key-unit "n9") "Map")
     "$ref")))

  (define duplicate-json
    (hash-set future-json 'nodes
              (list (car (jref future-json 'nodes))
                    (car (jref future-json 'nodes)))))
  (check-exn exn:fail?
             (lambda () (jsexpr->go-ast-unit duplicate-json
                                             #:schema fake-schema
                                             #:allow-unknown? #t)))

  (define duplicate-source-json
    (hash-set provenance-json
              'sources
              (list (hasheq 'id 'input 'name "first.grk" 'size 12)
                    (hasheq 'id "input" 'name "second.grk" 'size 12))))
  (check-exn exn:fail?
             (lambda ()
               (jsexpr->go-ast-unit duplicate-source-json
                                    #:schema fake-schema
                                    #:allow-unknown? #t)))

  (define active-schema (load-go-schema))
  (define typo-ident
    (make-ir-node "n50" "go/ast" "Ident"
                  '( ("Name" . "x") ("Typo" . "not-an-official-field") )))
  (check-exn exn:fail?
             (lambda () (make-go-ast-unit typo-ident #:schema active-schema)))
  (check-exn exn:fail?
             (lambda () (make-go-ast-unit future #:schema active-schema)))

  (define valid-extension-unit
    (struct-copy go-ast-unit future-unit
                 [extensions
                  (list (hasheq 'namespace "gorack/core"
                                'version 1
                                'value (hasheq 'enabled #t)))]))
  (check-eq? (validate-go-ast-unit valid-extension-unit
                                   #:schema fake-schema
                                   #:allow-unknown? #t)
             valid-extension-unit)
  (check-exn exn:fail?
             (lambda ()
               (validate-go-ast-unit
                (struct-copy go-ast-unit future-unit
                             [extensions (list (hasheq 'bogus 1))])
                #:schema fake-schema
                #:allow-unknown? #t))))
