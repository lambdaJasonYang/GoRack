#lang racket/base

(require json
         racket/format
         racket/list
         racket/match
         racket/runtime-path
         racket/string
         "node.rkt")

(provide (struct-out schema-identity)
         (struct-out go-field-schema)
         (struct-out go-node-schema)
         (struct-out go-token-schema)
         (struct-out go-enum-schema)
         (struct-out go-schema)
         default-go-schema-path
         load-go-schema
         current-go-schema
         go-schema-node-ref
         go-schema-field-ref
         go-schema-token-ref
         validate-ir-node)

(struct schema-identity
  (go-version schema-hash ast-package-hash token-package-hash)
  #:transparent)

(struct go-field-schema
  (name go-type kind optional? interface element enum-type documentation)
  #:transparent)

(struct go-node-schema
  (name namespace interfaces fields field-index documentation)
  #:transparent)

(struct go-token-schema (name spelling value) #:transparent)
(struct go-enum-schema (name go-type flag? values) #:transparent)

(struct go-schema
  (format format-version identity nodes node-index tokens token-index enums exclusions raw)
  #:transparent)

(define-runtime-path default-go-schema-path
  "../../go-bridge/internal/generated/schema.json")

(define missing (gensym 'missing))
(define absent (gensym 'absent))

(define (jref object key [default missing])
  (define string-key (if (symbol? key) (symbol->string key) key))
  (define symbol-key (if (symbol? key) key (string->symbol key)))
  (cond
    [(and (hash? object) (hash-has-key? object symbol-key))
     (hash-ref object symbol-key)]
    [(and (hash? object) (hash-has-key? object string-key))
     (hash-ref object string-key)]
    [(eq? default missing)
     (raise-arguments-error 'load-go-schema
                            "generated schema is missing a required property"
                            "property" string-key)]
    [else default]))

(define (as-string value [default ""])
  (cond [(string? value) value]
        [(symbol? value) (symbol->string value)]
        [(or (void? value) (eq? value (json-null))) default]
        [else (~a value)]))

(define (parse-field raw)
  ;; The loader accepts the normalized flat format and the earlier nested
  ;; {type:{kind:...}} spelling so schema tooling can evolve independently.
  (define type-object (jref raw 'type #hasheq()))
  (define (property name [default #f])
    (jref raw name (jref type-object name default)))
  (define required-value (jref raw 'required absent))
  (define optional-value (jref raw 'optional absent))
  (define optional?
    (cond [(not (eq? optional-value absent)) (and optional-value #t)]
          [(not (eq? required-value absent)) (not required-value)]
          [else #f]))
  (go-field-schema
   (as-string (jref raw 'name))
   (as-string (jref raw 'goType (jref raw 'go-type "")))
   (string->symbol (as-string (property 'kind)))
   optional?
   (let ([value (property 'interface #f)]) (and value (as-string value)))
   (let ([value (property 'element #f)]) (and value (as-string value)))
   (let ([value (property 'enumType (property 'enum-type #f))])
     (and value (as-string value)))
   (as-string (jref raw 'documentation (jref raw 'doc "")))))

(define (parse-node raw)
  (define fields (map parse-field (jref raw 'fields '())))
  (define index
    (for/hash ([field (in-list fields)])
      (values (go-field-schema-name field) field)))
  (go-node-schema
   (as-string (jref raw 'name))
   (as-string (jref raw 'namespace "go/ast"))
   (map as-string (jref raw 'interfaces '()))
   fields
   index
   (as-string (jref raw 'documentation (jref raw 'doc "")))))

(define (parse-token raw)
  (go-token-schema
   (as-string (jref raw 'name))
   (as-string (jref raw 'spelling (jref raw 'lexeme "")))
   (jref raw 'value #f)))

(define (parse-enum raw)
  (define values
    (for/list ([value (in-list (jref raw 'values '()))])
      (cond
        [(hash? value)
         (cons (as-string (jref value 'name)) (jref value 'value #f))]
        [else (cons (as-string value) #f)])))
  (go-enum-schema
   (as-string (jref raw 'name))
   (as-string (jref raw 'goType (jref raw 'go-type "")))
   (and (jref raw 'flag (jref raw 'flags #f)) #t)
   values))

(define (load-go-schema [path default-go-schema-path])
  (define raw
    (call-with-input-file path read-json))
  (define identity-raw (jref raw 'identity (jref raw 'goSchema #hasheq())))
  (define identity
    (schema-identity
     (as-string (jref identity-raw 'goVersion (jref raw 'goVersion "")))
     (as-string (jref identity-raw 'schemaHash (jref raw 'schemaHash "")))
     (as-string (jref identity-raw 'astPackageHash (jref raw 'astPackageHash "")))
     (as-string (jref identity-raw 'tokenPackageHash (jref raw 'tokenPackageHash "")))))
  (define nodes (map parse-node (jref raw 'nodes '())))
  (define tokens (map parse-token (jref raw 'tokens '())))
  (define enums (map parse-enum (jref raw 'enums '())))
  (when (null? nodes)
    (raise-arguments-error 'load-go-schema
                           "generated schema contains no go/ast nodes"
                           "path" path))
  (for ([token (in-list tokens)])
    (register-token! (go-token-schema-name token)
                     (go-token-schema-spelling token)))
  (go-schema
   (as-string (jref raw 'format "gorack-go-ast-schema"))
   (jref raw 'formatVersion 1)
   identity
   nodes
   (for/hash ([node (in-list nodes)])
     (values (go-node-schema-name node) node))
   tokens
   (for/hash ([token (in-list tokens)])
     (values (go-token-schema-name token) token))
   enums
   (jref raw 'excludedFields '())
   raw))

(define current-go-schema
  (make-parameter #f))

(define (schema-or-current maybe-schema)
  (or maybe-schema
      (current-go-schema)
      (let ([loaded (load-go-schema)])
        (current-go-schema loaded)
        loaded)))

(define (go-schema-node-ref kind [schema #f] [default missing])
  (define found
    (hash-ref (go-schema-node-index (schema-or-current schema)) kind missing))
  (cond [(not (eq? found missing)) found]
        [(not (eq? default missing)) default]
        [else (raise-arguments-error 'go-schema-node-ref
                                     "unknown go/ast node kind"
                                     "kind" kind)]))

(define (go-schema-field-ref kind field [schema #f] [default missing])
  (define node (go-schema-node-ref kind schema #f))
  (define found
    (and node (hash-ref (go-node-schema-field-index node) field missing)))
  (cond [(and found (not (eq? found missing))) found]
        [(not (eq? default missing)) default]
        [else (raise-arguments-error 'go-schema-field-ref
                                     "unknown field for go/ast node"
                                     "kind" kind
                                     "field" field)]))

(define (go-schema-token-ref name [schema #f] [default missing])
  (define canonical (normalize-token-name name))
  (define found
    (hash-ref (go-schema-token-index (schema-or-current schema)) canonical missing))
  (cond [(not (eq? found missing)) found]
        [(not (eq? default missing)) default]
        [else (raise-arguments-error 'go-schema-token-ref
                                     "unknown Go token name"
                                     "token" canonical)]))

(define (field-child-valid? schema field value)
  (cond
    [(ir-ref? value) #t]
    [(not (ir-node? value)) #f]
    [(not (string=? (ir-node-namespace value) "go/ast")) #f]
    [(go-field-schema-element field)
     (string=? (ir-node-kind value) (go-field-schema-element field))]
    [(go-field-schema-interface field)
     (define child-schema (go-schema-node-ref (ir-node-kind value) schema #f))
     ;; A newer official node is retained by an older generic tool even though
     ;; that tool cannot prove its marker interface locally.
     (or (not child-schema)
         (member (go-field-schema-interface field)
                 (go-node-schema-interfaces child-schema)
                 string=?))]
    [else #t]))

(define (field-value-valid? schema field value)
  (define kind (go-field-schema-kind field))
  (cond
    [(ir-null? value)
     (or (go-field-schema-optional? field)
         (memq kind '(position node-list node-map)))]
    [(not value)
     (or (go-field-schema-optional? field)
         (memq kind '(bool position node-list node-map)))]
    [(eq? kind 'node) (field-child-valid? schema field value)]
    [(eq? kind 'node-list)
     (and (list? value)
          (andmap (lambda (v) (field-child-valid? schema field v)) value))]
    [(eq? kind 'node-map)
     (and (hash? value)
          (for/and ([v (in-hash-values value)])
            (field-child-valid? schema field v)))]
    [(eq? kind 'position) (ast-position? value)]
    [(eq? kind 'token) (or (ir-token? value) (symbol? value) (string? value))]
    [(eq? kind 'enum) (or (enum-value? value) (symbol? value) (string? value) (list? value))]
    [(eq? kind 'string) (string? value)]
    [(eq? kind 'bool) (boolean? value)]
    [(eq? kind 'int) (exact-integer? value)]
    [else #t]))

(define (validate-ir-node node #:schema [schema #f] #:allow-unknown? [allow-unknown? #t])
  (unless (ir-node? node)
    (raise-argument-error 'validate-ir-node "ir-node?" node))
  (define schema* (schema-or-current schema))
  (unless (string=? (ir-node-namespace node) "go/ast")
    (raise-arguments-error 'validate-ir-node
                           "node is not in the generated Go AST namespace"
                           "namespace" (ir-node-namespace node)))
  (define node-schema (go-schema-node-ref (ir-node-kind node) schema* #f))
  (cond
    [(not node-schema)
     (unless allow-unknown?
       (raise-arguments-error 'validate-ir-node
                              "node kind is not present in the active Go schema"
                              "kind" (ir-node-kind node)))]
    [else
     ;; Unknown fields are preserved only when the unit carries a different
     ;; schema identity and validation is therefore skipped by the envelope
     ;; reader. A node claiming the active schema must not smuggle a typo or
     ;; future field past the Racket boundary for the Go decoder to discard.
     (for ([field-name (in-hash-keys (ir-node-fields node))])
       (unless (hash-has-key? (go-node-schema-field-index node-schema) field-name)
         (raise-arguments-error 'validate-ir-node
                                "field is not present in the active Go schema"
                                "kind" (ir-node-kind node)
                                "field" field-name)))
     (for ([field (in-list (go-node-schema-fields node-schema))])
       (define present? (ir-node-has-field? node (go-field-schema-name field)))
       (when (and (not present?) (not (go-field-schema-optional? field)))
         (raise-arguments-error 'validate-ir-node
                                "required Go AST field is absent"
                                "kind" (ir-node-kind node)
                                "field" (go-field-schema-name field)))
       (when present?
         (define value (ir-node-field node (go-field-schema-name field)))
         (unless (field-value-valid? schema* field value)
           (raise-arguments-error 'validate-ir-node
                                  "Go AST field has the wrong value category"
                                  "kind" (ir-node-kind node)
                                  "field" (go-field-schema-name field)
                                  "expected" (go-field-schema-kind field)
                                  "value" value))))])
  node)
