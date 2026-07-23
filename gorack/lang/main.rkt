#lang racket
(require syntax/parse
         syntax/parse/define
         racket/string
         (for-syntax syntax/parse racket/list racket/string)
         racket/match
         racket/list
         (only-in racket #%app [> racket:>])
         "../go-kernel/node.rkt"
         "../go-kernel/generated/match.rkt"
         "../go-kernel/generated/tokens.rkt"
         "../go-kernel/wire.rkt")

;; Public #lang gorack surface. Internal AST helpers stay private.
(provide
  (rename-out [gorack-#%module-begin #%module-begin]
              [gorack-#%top #%top]
              [gorack-#%top #%top-interaction]
              [gorack-#%datum #%datum]
              [gorack-#%app #%app]
              [gorack-if if]
              [gorack-for for]
              [gorack-map map]
              [gorack-struct struct]
              [gorack-interface interface]
              [gorack-go go]
              [gorack-defer defer]
              [gorack-break break]
              [gorack-continue continue]
              [gorack-and and]
              [gorack-or or]
              [gorack-plus +]
              [gorack-minus -]
              [gorack-times *]
              [gorack-amp &]
              [gorack-define :=]
              [gorack-assign =]
              [gorack-add-assign +=]
              [gorack-sub-assign -=]
              [gorack-mul-assign *=]
              [gorack-quo-assign /=]
              [gorack-rem-assign %=]
              [gorack-and-assign &=]
              [gorack-or-assign bit-or=]
              [gorack-xor-assign ^=]
              [gorack-shl-assign <<=]
              [gorack-shr-assign >>=]
              [gorack-and-not-assign &^=])
  package import
  defn fn
  type type-alias var var= const=
  array slice ptr chan chan-send chan-recv
  sel index index-list slice-expr type-assert
  composite kv tag rawtag go-literal nil spread
  ! ~ <- / % bit-or xor and-not << >> == != < <= > >=
  return block for-range switch select type-switch
  send inc dec label goto fallthrough decl
  write-go-kernel-json write-go-kernel-json-file
  (all-from-out "../go-kernel/generated/tokens.rkt"))

;; Emit the versioned Go-kernel graph envelope.
(define (write-go-kernel-json n)
  (write-go-ast-json (current-output-port) n))

(define (write-go-kernel-json-file path n)
  (write-go-ast-json-file path n))


(begin-for-syntax
  ;; Surface form names are still valid Go identifiers in operand position.
  ;; For example, a range variable named `index` must not evaluate to the
  ;; Gorack `index` constructor when used as an argument.
  (define surface-identifier-names
    '(package import defn fn type type-alias var var= const=
      array slice ptr chan chan-send chan-recv sel index index-list
      slice-expr type-assert composite kv tag rawtag go-literal nil spread
      return block for-range switch select type-switch send inc dec label
      goto fallthrough decl if for map struct interface go defer break
      continue and or bit-or xor and-not))

  (define surface-runtime-head-names
    '(array slice map chan chan-send chan-recv index index-list slice-expr
      type-assert kv spread return block))

  (define (go-identifier-position? id)
    (or (not (identifier-binding id))
        (memq (syntax-e id) surface-identifier-names)))

  ;; Bare Go identifiers become AST identifiers before Racket can resolve a
  ;; same-named surface helper. Other expressions pass through unchanged.
  (define-syntax-class exprish
    #:attributes (datum)
    (pattern id:id
      #:when (go-identifier-position? #'id)
      #:with datum #`(->qualified-ident* '#,(syntax-e #'id)))
    (pattern e:expr
      #:with datum #'e)))
;; ---------- compile-time helpers ----------
(begin-for-syntax
  (define (square-bracketed? stx)
    (eqv? (syntax-property stx 'paren-shape) #\[))

  ;; Recognize the simple statement forms accepted by the compact three-part
  ;; loop spelling. Arbitrary init/post expressions remain available through
  ;; explicit (init ...) and (post ...) clauses.
  (define for-simple-statement-heads
    '(:= = += -= *= /= %= &= bit-or= ^= <<= >>= &^= inc dec send))

  (define (for-simple-statement? stx)
    (syntax-parse stx
      [(head:id . _)
       (memq (syntax-e #'head) for-simple-statement-heads)]
      [_ #f]))

  ;; An identifier like `int`/`error` → (->ident* 'int), arbitrary expr passes through.
  (define-syntax-class typeish
    #:attributes (datum)
    (pattern id:id
      #:with datum #`(->qualified-ident* '#,(syntax-e #'id)))
    ;; Optional: accept string type names as a future surface convenience.
    (pattern e:expr
      #:with datum #'e))

  ;; Binding entries use square brackets. This keeps compound type forms such
  ;; as (slice byte) unambiguous in result lists.
  (define-syntax-class param-pair
    #:attributes (name type datum)
    (pattern (~and whole (nm:id ty:typeish))
      #:when (square-bracketed? #'whole)
      #:with name #'nm
      #:with type #'ty.datum
      #:with datum #`(list '#,(syntax-e #'nm) ty.datum)))

  ;; Named results use [name Type]; every other expression is an unnamed type.
  (define-syntax-class result-item
    #:attributes (datum)
    (pattern (~and whole (nm:id ty:typeish))
      #:when (square-bracketed? #'whole)
      #:with datum #`(list '#,(syntax-e #'nm) ty.datum))
    (pattern ty:typeish
      #:with datum #'ty.datum))

  ;; Canonical function signature: (-> ([x T] ...) (Result ...)).
  ;; The parameter/result groups may use either Racket delimiter; named
  ;; bindings themselves use square brackets.
  (define-syntax-class go-signature
    #:attributes (params results)
    #:datum-literals (->)
    (pattern (-> (p:param-pair ...) (r:result-item ...))
      #:with params #'(list p.datum ...)
      #:with results #'(list r.datum ...)))
)


(define-syntax (go-expr stx)
  (syntax-parse stx
    [(_ value:exprish) #'value.datum]))

(define-syntax (gorack-#%datum stx)
  (syntax-parse stx
    ;; booleans: #t / #f
    [(_ . b:boolean)
     #`(mkbool #,(syntax-e #'b))]

    ;; exact integers → INT
    [(_ . n:exact-integer)
     #`(mkint #,(syntax-e #'n))]

    ;; non-integer reals (incl. rationals) → FLOAT
    [(_ . n:number)
     #:when (and (real? (syntax-e #'n))
                 (not (exact-integer? (syntax-e #'n))))
     #`(mkfloat #,(exact->inexact (syntax-e #'n)))]

    ;; chars: #\a, #\newline, etc.
    [(_ . c:char)
     #`(mkrune #,(syntax-e #'c))]

    ;; strings: "hi"
    [(_ . s:string)
     #`(mkstr #,(syntax-e #'s))]

    ;; anything else
    [(_ . other)
     (raise-syntax-error '#%datum
       (format "unsupported literal in gorack: ~a" (syntax-e #'other))
       #'other)]))


;; ---------- #%datum: turn Racket literals into Go AST literals ----------


;; ─────────────────────────────────────────────────────────────
;; Node-ID allocator (monotone per module run, shared with the generic IR)
;; ─────────────────────────────────────────────────────────────
(define reset-ids! reset-node-ids!)
(define new-id! fresh-node-id)

;; ---------------- helpers (runtime) ----------------

(define (->ident* s)
  (cond
    [(symbol? s) (go:Ident #:id (new-id!) #:name-pos #f #:name (symbol->string s))]
    [(string? s) (go:Ident #:id (new-id!) #:name-pos #f #:name s)]
    [else (error '->ident*
            (format "expected symbol/string, got ~v" s))]))

(define (->qualified-ident* value)
  (define text
    (cond
      [(symbol? value) (symbol->string value)]
      [(string? value) value]
      [else (error '->qualified-ident*
                   (format "expected symbol/string, got ~v" value))]))
  (define parts (string-split text "."))
  (if (or (null? parts) (null? (cdr parts)))
      (->ident* text)
      (for/fold ([base (->ident* (car parts))])
                ([field (in-list (cdr parts))])
        (go:SelectorExpr #:id (new-id!)
                         #:x base
                         #:sel (->ident* field)))))


;; ── Decl coercions ──────────────────────────────────────────────────────────
(define (->decl d)
  (cond [(or (go:GenDecl? d) (go:FuncDecl? d) (go:BadDecl? d)) d]
        [else (error '->decl (format "not a declaration: ~v" d))]))

(define (decl->stmt d)
  (go:DeclStmt #:id (new-id!) #:decl (->decl d)))

(define (->expr v)
  (cond
    [(or (go:Ident? v) (go:BasicLit? v)
         (go:CallExpr? v) (go:SelectorExpr? v)
         (go:UnaryExpr? v) (go:BinaryExpr? v)
         (go:CompositeLit? v) (go:ParenExpr? v)
         (go:IndexExpr? v) (go:IndexListExpr? v)
         (go:SliceExpr? v) (go:TypeAssertExpr? v)
         (go:KeyValueExpr? v) (go:BadExpr? v)
         (go:StarExpr? v) (go:Ellipsis? v)
         (go:ArrayType? v) (go:StructType? v)
         (go:MapType? v) (go:ChanType? v)
         (go:FuncLit? v) 
         (go:FuncType? v) (go:InterfaceType? v))
     v]
    [(symbol? v)   (->qualified-ident* v)]
    [(string? v)   (->qualified-ident* v)]
    [(boolean? v)  (mkbool v)]
    [(exact-integer? v) (mkint v)     ]
    [(real? v) (mkfloat v)]
    [else (error '->expr (format "not an expression: ~v" v))]))
(define (expr->stmt n)
  (cond [(or (go:DeclStmt? n) (go:EmptyStmt? n) (go:LabeledStmt? n)
             (go:ExprStmt? n) (go:SendStmt? n) (go:IncDecStmt? n)
             (go:AssignStmt? n) (go:GoStmt? n) (go:DeferStmt? n)
             (go:ReturnStmt? n) (go:BranchStmt? n) (go:BlockStmt? n)
             (go:IfStmt? n) (go:SwitchStmt? n) (go:TypeSwitchStmt? n)
             (go:SelectStmt? n) (go:ForStmt? n) (go:RangeStmt? n)
              (go:CaseClause? n) (go:CommClause? n) (go:BadStmt? n))
         n]
        [(go:GenDecl? n)  (decl->stmt n)]  
        [else (go:ExprStmt #:id (new-id!) #:x (->expr n))]))



(define (go-string s) (format "~s" s)) ; add quotes/escapes for Go string literal

(define (maybe-null xs)
  (cond [(not xs) #f]
        [(and (list? xs) (null? xs)) #f]
        [else xs]))

(define (file-with name decls imports)
  (go:File #:id (new-id!) #:doc #f #:package #f #:name (->ident* name) #:decls decls #:file-start #f #:file-end #f #:imports (maybe-null imports) #:comments #f #:go-version ""))
;; --------------- base exprs / literals ----------------


;; Literal constructors used by the surface reader.
(define (mkstr s)   (go:BasicLit #:id (new-id!) #:value-pos #f #:kind 'STRING #:value (go-string s)))
(define (mkint n)   (go:BasicLit #:id (new-id!) #:value-pos #f #:kind 'INT #:value (number->string n)))
(define (mkfloat x) (go:BasicLit #:id (new-id!) #:value-pos #f #:kind 'FLOAT #:value (number->string x)))
(define (mkbool b)  (->ident* (if b 'true 'false)))
(define (mkrune c)  (go:BasicLit #:id (new-id!) #:value-pos #f #:kind 'CHAR #:value (format "'~a'" c)))
(define (mkimag s)  (go:BasicLit #:id (new-id!) #:value-pos #f #:kind 'IMAG #:value s))

;; Preserve an official Go literal's spelling exactly. The surface reader
;; deliberately turns ordinary Racket literals into Go AST nodes.
;; Consuming KIND and VALUE in a macro prevents that useful datum behaviour
;; from wrapping an already-constructed BasicLit a second time.
(define-syntax (go-literal stx)
  (syntax-parse stx
    [(_ kind:id value:string)
     (define kind-symbol (syntax-e #'kind))
     (define literal-value (syntax-e #'value))
     #`(go:BasicLit #:id (new-id!) #:value-pos #f #:kind '#,kind-symbol #:value '#,literal-value)]))
;;-----------------------------------------------------------------------------
(define-syntax (sel stx)
  (syntax-parse stx
    [(_ x:expr field:id)
     #`(go:SelectorExpr #:id (new-id!) #:x (->expr x) #:sel (->ident* '#,(syntax-e #'field)))]))

;; Pass-through module-begin: expand user forms as a normal Racket module.


;; Build a Go call. A final (spread xs) marks a variadic call.
(define (spread xs) (list 'spread xs))

(define (make-go-call f args)
  (define spread?
    (and (pair? args)
         (pair? (last args))
         (eq? (car (last args)) 'spread)))
  (define args*
    (if spread?
        (append (drop-right args 1) (list (cadr (last args))))
        args))
  (go:CallExpr #:id (new-id!)
               #:fun (->expr f)
               #:lparen #f
               #:args (map ->expr args*)
               #:ellipsis (and spread? (ast-position "generated" 0))
               #:rparen #f))

;; Bound Gorack forms and helpers use normal Racket application. An unbound or
;; computed head is a Go callee, so (fmt.Println x) and (f x) create CallExprs.
(define-syntax (gorack-#%app stx)
  (syntax-parse stx
    [(_ f:id arg:exprish ...)
     (cond
       [(not (identifier-binding #'f))
        #'(make-go-call f (list arg.datum ...))]
       [(memq (syntax-e #'f) surface-runtime-head-names)
        #'(#%plain-app f arg.datum ...)]
       [else
        #'(#%plain-app f arg ...)])]
    [(_ f:expr arg:exprish ...)
     #'(make-go-call f (list arg.datum ...))]))

;; --------------- composite lits / indexing / slices / assertions ---------------

(define (index x i)       (go:IndexExpr #:id (new-id!) #:x (->expr x) #:lbrack #f #:index (->expr i) #:rbrack #f))
(define (index-list x . is) (go:IndexListExpr #:id (new-id!) #:x (->expr x) #:lbrack #f #:indices (map ->expr is) #:rbrack #f))

;; (slice-expr x low high [max])
(define (slice-expr x low high . rest)
  (go:SliceExpr #:id (new-id!) #:x (->expr x) #:lbrack #f #:low (and low (->expr low)) #:high (and high (->expr high)) #:max (and (pair? rest) (->expr (car rest))) #:slice3 (pair? rest) #:rbrack #f))

(define (type-assert x ty)
  (go:TypeAssertExpr #:id (new-id!) #:x (->expr x) #:lparen #f #:type (and ty (->expr ty)) #:rparen #f))

;; --------------- types ----------------

(define (array len elt)
  (go:ArrayType #:id (new-id!) #:lbrack #f #:len (and len (if (equal? (normalize-token-name len) go-token:ELLIPSIS) (go:Ellipsis #:id (new-id!) #:ellipsis #f #:elt #f) (->expr len))) #:elt (->expr elt)))

(define (slice elt)      (go:ArrayType #:id (new-id!) #:lbrack #f #:len #f #:elt (->expr elt)))
(define (gorack-map key value)  (go:MapType #:id (new-id!) #:map #f #:key (->expr key) #:value (->expr value)))

;; runtime (constructor) — renamed to avoid clashing with macro
;; -------- compile-time helper for struct fields --------
;; runtime helper: make a Go raw string literal with backticks
;; Backtick (raw) Go string for struct tags
(define (mkrawtag s)
  (go:BasicLit #:id (new-id!) #:value-pos #f #:kind 'STRING #:value (format "`~a`" s)))

;; -------- compile-time helper for struct fields (with optional tag) -----
;; Build tags from specs like:
;;   (tag (json fieldName #:omitempty) (db another_field #:omitempty))

(begin-for-syntax
  ;; unwrap syntax -> keyword -> "omitempty"
  (define (kw->opt k-stx)
    (keyword->string (syntax-e k-stx)))

  (define (id->s stx) (symbol->string (syntax-e stx)))

  (define (spec->frag stx)
    (syntax-parse stx
      ;; value as identifier
      [(k:id v:id opt:keyword ...)
       (define key  (id->s #'k))
       (define val  (id->s #'v))
       (define opts (for/list ([o (in-list (syntax->list #'(opt ...)))])
                      (kw->opt o)))
       (define opt-part (if (null? opts) "" (string-append "," (string-join opts ","))))
       (format "~a:\"~a~a\"" key val opt-part)]

      ;; value as string
      [(k:id v:string opt:keyword ...)
       (define key  (id->s #'k))
       (define val  (syntax-e #'v))
       (define opts (for/list ([o (in-list (syntax->list #'(opt ...)))])
                      (kw->opt o)))
       (define opt-part (if (null? opts) "" (string-append "," (string-join opts ","))))
       (format "~a:\"~a~a\"" key val opt-part)])))

(define-syntax (tag stx)
  (syntax-parse stx
    [(_ spec ...+)
     (define pieces (map spec->frag (syntax->list #'(spec ...))))
     (define all (string-join pieces " "))
     #`(mkrawtag #,all)]))
;; Raw tag without escaping, using a vertical-bar symbol:
;;   (rawtag |json:"field" db:"x,omitempty"|)
(define-syntax (rawtag stx)
  (syntax-parse stx
    [(_ s:id)   #`(mkrawtag #,(symbol->string (syntax-e #'s)))]
    [(_ s:string)  #'(mkrawtag s)]))

(begin-for-syntax
  (define-syntax-class struct-field
    #:attributes (datum)

    ;; ----- many names (identifiers) + TAG -----
    ;; tag as plain string literal  → mkrawtag
    (pattern ([names:id ...] ty:typeish tag:string)
      #:with datum
      #`(go:Field #:id (new-id!) #:doc #f #:names (list (->ident* 'names) ...) #:type (->expr ty.datum) #:tag (mkrawtag tag) #:comment #f))
    ;; tag as expression (e.g., (tag …) or (rawtag …)) → use directly
    (pattern ([names:id ...] ty:typeish tagx:expr)
      #:with datum
      #`(go:Field #:id (new-id!) #:doc #f #:names (list (->ident* 'names) ...) #:type (->expr ty.datum) #:tag tagx #:comment #f))

    ;; ----- many names (identifiers), no tag -----
    (pattern ([names:id ...] ty:typeish)
      #:with datum
      #`(go:Field #:id (new-id!) #:doc #f #:names (list (->ident* 'names) ...) #:type (->expr ty.datum) #:tag #f #:comment #f))

    ;; ----- many names (strings) + TAG -----
    (pattern ([names:string ...] ty:typeish tag2:string)
      #:with datum
      #`(go:Field #:id (new-id!) #:doc #f #:names (list (->ident* names) ...) #:type (->expr ty.datum) #:tag (mkrawtag tag2) #:comment #f))
    (pattern ([names:string ...] ty:typeish tag2x:expr)
      #:with datum
      #`(go:Field #:id (new-id!) #:doc #f #:names (list (->ident* names) ...) #:type (->expr ty.datum) #:tag tag2x #:comment #f))

    ;; ----- many names (strings), no tag -----
    (pattern ([names:string ...] ty:typeish)
      #:with datum
      #`(go:Field #:id (new-id!) #:doc #f #:names (list (->ident* names) ...) #:type (->expr ty.datum) #:tag #f #:comment #f))

    ;; ----- single name (identifier) + TAG -----
    (pattern (name:id ty:typeish tag3:string)
      #:with datum
      #`(go:Field #:id (new-id!) #:doc #f #:names (list (->ident* 'name)) #:type (->expr ty.datum) #:tag (mkrawtag tag3) #:comment #f))
    (pattern (name:id ty:typeish tag3x:expr)
      #:with datum
      #`(go:Field #:id (new-id!) #:doc #f #:names (list (->ident* 'name)) #:type (->expr ty.datum) #:tag tag3x #:comment #f))

    ;; ----- single name (identifier), no tag -----
    (pattern (name:id ty:typeish)
      #:with datum
      #`(go:Field #:id (new-id!) #:doc #f #:names (list (->ident* 'name)) #:type (->expr ty.datum) #:tag #f #:comment #f))

    ;; ----- single name (string) + TAG -----
    (pattern (name:string ty:typeish tag4:string)
      #:with datum
      #`(go:Field #:id (new-id!) #:doc #f #:names (list (->ident* name)) #:type (->expr ty.datum) #:tag (mkrawtag tag4) #:comment #f))
    (pattern (name:string ty:typeish tag4x:expr)
      #:with datum
      #`(go:Field #:id (new-id!) #:doc #f #:names (list (->ident* name)) #:type (->expr ty.datum) #:tag tag4x #:comment #f))

    ;; ----- single name (string), no tag -----
    (pattern (name:string ty:typeish)
      #:with datum
      #`(go:Field #:id (new-id!) #:doc #f #:names (list (->ident* name)) #:type (->expr ty.datum) #:tag #f #:comment #f))

    ;; ----- embedded + TAG -----
    (pattern (ty:typeish tag5:string)
      #:with datum
      #`(go:Field #:id (new-id!) #:doc #f #:names '() #:type (->expr ty.datum) #:tag (mkrawtag tag5) #:comment #f))
    (pattern (ty:typeish tag5x:expr)
      #:with datum
      #`(go:Field #:id (new-id!) #:doc #f #:names '() #:type (->expr ty.datum) #:tag tag5x #:comment #f))

    ;; ----- embedded, no tag -----
    (pattern ty:typeish
      #:with datum
      #`(go:Field #:id (new-id!) #:doc #f #:names '() #:type (->expr ty.datum) #:tag #f #:comment #f))))

;; -------- struct macro (expands to a go:StructType) --------
(define-syntax (gorack-struct stx)
  (syntax-parse stx
    [(_ f:struct-field ...+)
     #'(go:StructType #:id (new-id!) #:struct #f #:fields (go:FieldList #:id (new-id!) #:opening #f #:list (list f.datum ...) #:closing #f) #:incomplete #f)]))



;; --------------- composite lits / indexing / slices / assertions ---------------

;; (composite Type [ elt ... ]) with elts either expr or (kv key val)
;; add this to ->expr dispatch:
(define (kv k v) (go:KeyValueExpr #:id (new-id!) #:key (->expr k) #:colon #f #:value (->expr v)))

(define (make-composite ty elts)
  (go:CompositeLit #:id (new-id!) #:type (->expr ty) #:lbrace #f #:elts (for/list ([e elts])
     (cond [(go:KeyValueExpr? e) e]
           [(and (pair? e) (eq? (car e) 'kv)) (apply kv (cdr e))]
           [else (->expr e)])) #:rbrace #f #:incomplete #f))

;; Brackets are grouping syntax in the surface language, not a call to an
;; unbound `list` identifier. The second clause retains the ordinary variadic
;; spelling accepted by the surface syntax.
(define-syntax (composite stx)
  (syntax-parse stx
    [(_ ty:expr grouped)
     #:when (eqv? (syntax-property #'grouped 'paren-shape) #\[)
     (syntax-parse #'grouped
       [(elt:expr ...)
        #'(make-composite ty (list elt ...))])]
    [(_ ty:expr elt:expr ...)
     #'(make-composite ty (list elt ...))]))





;; --------------- operators ---------------

(define (unary op x)
  (go:UnaryExpr #:id (new-id!) #:op-pos #f #:op op #:x (->expr x)))

(define (binary op x y)
  (go:BinaryExpr #:id (new-id!) #:x (->expr x) #:op-pos #f #:op op #:y (->expr y)))

(define (binary-chain op xs)
  (unless (and (list? xs) (pair? xs) (pair? (cdr xs)))
    (raise-arguments-error 'binary-chain "expected at least two operands" "operands" xs))
  (for/fold ([acc (->expr (car xs))]) ([item (in-list (cdr xs))])
    (go:BinaryExpr #:id (new-id!) #:x acc #:op-pos #f #:op op #:y (->expr item))))

(define-syntax-rule (! x) (unary go-token:NOT (go-expr x)))
(define-syntax-rule (~ x) (unary go-token:TILDE (go-expr x)))
(define-syntax-rule (<- x) (unary go-token:ARROW (go-expr x)))

(define-syntax (gorack-plus stx)
  (syntax-parse stx
    [(_ x:expr) #'(unary go-token:ADD (go-expr x))]
    [(_ x:expr y:expr more:expr ...)
     #'(binary-chain go-token:ADD (list (go-expr x) (go-expr y) (go-expr more) ...))]))

(define-syntax (gorack-minus stx)
  (syntax-parse stx
    [(_ x:expr) #'(unary go-token:SUB (go-expr x))]
    [(_ x:expr y:expr more:expr ...)
     #'(binary-chain go-token:SUB (list (go-expr x) (go-expr y) (go-expr more) ...))]))

(define-syntax (gorack-times stx)
  (syntax-parse stx
    [(_ x:expr) #'(go:StarExpr #:id (new-id!) #:star #f #:x (->expr (go-expr x)))]
    [(_ x:expr y:expr more:expr ...)
     #'(binary-chain go-token:MUL (list (go-expr x) (go-expr y) (go-expr more) ...))]))

(define-syntax (gorack-amp stx)
  (syntax-parse stx
    [(_ x:expr) #'(unary go-token:AND (go-expr x))]
    [(_ x:expr y:expr more:expr ...)
     #'(binary-chain go-token:AND (list (go-expr x) (go-expr y) (go-expr more) ...))]))

(define-syntax-rule (/ x y more ...)
  (binary-chain go-token:QUO (list (go-expr x) (go-expr y) (go-expr more) ...)))
(define-syntax-rule (% x y more ...)
  (binary-chain go-token:REM (list (go-expr x) (go-expr y) (go-expr more) ...)))
(define-syntax-rule (bit-or x y more ...)
  (binary-chain go-token:OR (list (go-expr x) (go-expr y) (go-expr more) ...)))
(define-syntax-rule (xor x y more ...)
  (binary-chain go-token:XOR (list (go-expr x) (go-expr y) (go-expr more) ...)))
(define-syntax-rule (and-not x y more ...)
  (binary-chain go-token:AND_NOT (list (go-expr x) (go-expr y) (go-expr more) ...)))
(define-syntax-rule (<< x y) (binary go-token:SHL (go-expr x) (go-expr y)))
(define-syntax-rule (>> x y) (binary go-token:SHR (go-expr x) (go-expr y)))
(define-syntax-rule (== x y) (binary go-token:EQL (go-expr x) (go-expr y)))
(define-syntax-rule (!= x y) (binary go-token:NEQ (go-expr x) (go-expr y)))
(define-syntax-rule (< x y) (binary go-token:LSS (go-expr x) (go-expr y)))
(define-syntax-rule (<= x y) (binary go-token:LEQ (go-expr x) (go-expr y)))
(define-syntax-rule (> x y) (binary go-token:GTR (go-expr x) (go-expr y)))
(define-syntax-rule (>= x y) (binary go-token:GEQ (go-expr x) (go-expr y)))
(define-syntax-rule (gorack-and x y more ...)
  (binary-chain go-token:LAND (list (go-expr x) (go-expr y) (go-expr more) ...)))
(define-syntax-rule (gorack-or x y more ...)
  (binary-chain go-token:LOR (list (go-expr x) (go-expr y) (go-expr more) ...)))

;; Assignment operators are themselves the public heads. There is no public
;; `assign` compatibility form.
(define-syntax (make-assignment-form stx)
  (syntax-parse stx
    ;; Multiple left- and right-hand values.
    [(_ token:id lhs-group rhs-group)
     #:when (and (square-bracketed? #'lhs-group)
                 (square-bracketed? #'rhs-group))
     (syntax-parse #'(lhs-group rhs-group)
       [((lhs:exprish ...+) (rhs:exprish ...+))
        #`(go:AssignStmt #:id (new-id!)
                         #:lhs (list (->expr lhs.datum) ...)
                         #:tok-pos #f
                         #:tok '#,(syntax-e #'token)
                         #:rhs (list (->expr rhs.datum) ...))]
       [_ (raise-syntax-error 'assignment
                              "left- and right-hand groups must be non-empty"
                              stx)])]
    ;; Multiple targets receiving the single result of one expression, as in
    ;; (:= [value ok] (<- channel)) or (:= [value err] (lookup key)).
    [(_ token:id lhs-group rhs:exprish)
     #:when (square-bracketed? #'lhs-group)
     (syntax-parse #'lhs-group
       [(lhs:exprish ...+)
        #`(go:AssignStmt #:id (new-id!)
                         #:lhs (list (->expr lhs.datum) ...)
                         #:tok-pos #f
                         #:tok '#,(syntax-e #'token)
                         #:rhs (list (->expr rhs.datum)))]
       [_ (raise-syntax-error 'assignment
                              "left-hand group must be non-empty"
                              #'lhs-group)])]
    ;; Ordinary single-target assignment.
    [(_ token:id lhs:exprish rhs:exprish)
     #`(go:AssignStmt #:id (new-id!)
                      #:lhs (list (->expr lhs.datum))
                      #:tok-pos #f
                      #:tok '#,(syntax-e #'token)
                      #:rhs (list (->expr rhs.datum)))]))

(define-syntax-rule (gorack-define args ...) (make-assignment-form DEFINE args ...))
(define-syntax-rule (gorack-assign args ...) (make-assignment-form ASSIGN args ...))
(define-syntax-rule (gorack-add-assign args ...) (make-assignment-form ADD_ASSIGN args ...))
(define-syntax-rule (gorack-sub-assign args ...) (make-assignment-form SUB_ASSIGN args ...))
(define-syntax-rule (gorack-mul-assign args ...) (make-assignment-form MUL_ASSIGN args ...))
(define-syntax-rule (gorack-quo-assign args ...) (make-assignment-form QUO_ASSIGN args ...))
(define-syntax-rule (gorack-rem-assign args ...) (make-assignment-form REM_ASSIGN args ...))
(define-syntax-rule (gorack-and-assign args ...) (make-assignment-form AND_ASSIGN args ...))
(define-syntax-rule (gorack-or-assign args ...) (make-assignment-form OR_ASSIGN args ...))
(define-syntax-rule (gorack-xor-assign args ...) (make-assignment-form XOR_ASSIGN args ...))
(define-syntax-rule (gorack-shl-assign args ...) (make-assignment-form SHL_ASSIGN args ...))
(define-syntax-rule (gorack-shr-assign args ...) (make-assignment-form SHR_ASSIGN args ...))
(define-syntax-rule (gorack-and-not-assign args ...) (make-assignment-form AND_NOT_ASSIGN args ...))

;; --------------- statements ----------------

(define (return . xs)
  (go:ReturnStmt #:id (new-id!) #:return #f #:results (map ->expr xs)))

(define (block . body)
  (go:BlockStmt #:id (new-id!) #:lbrace #f #:list (map expr->stmt body) #:rbrace #f))

;; `if` optionally accepts a headed initializer: (if (init stmt) cond ...).
(define-syntax (gorack-if stx)
  (syntax-parse stx
    #:datum-literals (begin init)
    ;; Initializer plus explicit multi-statement branches.
    [(_ (init init-expr:expr) cond:expr (begin thn:expr ...) (begin els:expr ...))
     #'(go:IfStmt #:id (new-id!) #:if #f #:init (expr->stmt init-expr) #:cond (->expr (go-expr cond)) #:body (block thn ...) #:else (block els ...))]
    [(_ (init init-expr:expr) cond:expr (begin thn:expr ...) els:expr ...+)
     #'(go:IfStmt #:id (new-id!) #:if #f #:init (expr->stmt init-expr) #:cond (->expr (go-expr cond)) #:body (block thn ...) #:else (block els ...))]
    [(_ (init init-expr:expr) cond:expr (begin thn:expr ...))
     #'(go:IfStmt #:id (new-id!) #:if #f #:init (expr->stmt init-expr) #:cond (->expr (go-expr cond)) #:body (block thn ...) #:else #f)]
    [(_ (init init-expr:expr) cond:expr thn:expr els:expr)
     #'(go:IfStmt #:id (new-id!) #:if #f #:init (expr->stmt init-expr) #:cond (->expr (go-expr cond)) #:body (block thn) #:else (block els))]
    [(_ (init init-expr:expr) cond:expr thn:expr ...+)
     #'(go:IfStmt #:id (new-id!) #:if #f #:init (expr->stmt init-expr) #:cond (->expr (go-expr cond)) #:body (block thn ...) #:else #f)]

    ;; Ordinary if forms.
    [(_ cond:expr (begin thn:expr ...) (begin els:expr ...))
     #'(go:IfStmt #:id (new-id!) #:if #f #:init #f #:cond (->expr (go-expr cond)) #:body (block thn ...) #:else (block els ...))]
    [(_ cond:expr (begin thn:expr ...) els:expr ...+)
     #'(go:IfStmt #:id (new-id!) #:if #f #:init #f #:cond (->expr (go-expr cond)) #:body (block thn ...) #:else (block els ...))]
    [(_ cond:expr (begin thn:expr ...))
     #'(go:IfStmt #:id (new-id!) #:if #f #:init #f #:cond (->expr (go-expr cond)) #:body (block thn ...) #:else #f)]
    [(_ cond:expr thn:expr els:expr)
     #'(go:IfStmt #:id (new-id!) #:if #f #:init #f #:cond (->expr (go-expr cond)) #:body (block thn) #:else (block els))]
    [(_ cond:expr thn:expr ...+)
     #'(go:IfStmt #:id (new-id!) #:if #f #:init #f #:cond (->expr (go-expr cond)) #:body (block thn ...) #:else #f)]))

(define-syntax (gorack-for stx)
  (syntax-parse stx
    #:datum-literals (init post forever)
    ;; Explicit clauses support any Go simple statement in init/post position.
    [(_ (init init-expr:expr) cond:expr (post post-expr:expr) body:expr ...+)
     #'(go:ForStmt #:id (new-id!) #:for #f #:init (expr->stmt init-expr) #:cond (->expr (go-expr cond)) #:post (expr->stmt post-expr) #:body (block body ...))]
    ;; Compact three-part loops are accepted when init and post have known
    ;; statement heads. This keeps condition loops with several body forms
    ;; unambiguous.
    [(_ init-expr:expr cond:expr post-expr:expr body:expr ...+)
     #:when (and (for-simple-statement? #'init-expr)
                 (for-simple-statement? #'post-expr))
     #'(go:ForStmt #:id (new-id!) #:for #f #:init (expr->stmt init-expr) #:cond (->expr (go-expr cond)) #:post (expr->stmt post-expr) #:body (block body ...))]
    ;; Explicit infinite loop.
    [(_ (forever) body:expr ...+)
     #'(go:ForStmt #:id (new-id!) #:for #f #:init #f #:cond #f #:post #f #:body (block body ...))]
    ;; Condition-only loop, with any number of body statements.
    [(_ cond:expr body:expr ...+)
     #'(go:ForStmt #:id (new-id!) #:for #f #:init #f #:cond (->expr (go-expr cond)) #:post #f #:body (block body ...))]))

;; Range bindings are headed assignments: (for-range (:= [k v] xs) body ...).
(define-syntax (for-range stx)
  (define (range-token op-stx)
    (case (syntax-e op-stx)
      [(:=) 'DEFINE]
      [(=) 'ASSIGN]
      [else (raise-syntax-error 'for-range "expected := or =" op-stx)]))
  (syntax-parse stx
    [(_ (op:id binding x:expr) body:expr ...+)
     #:when (and (memq (syntax-e #'op) '(:= =))
                 (square-bracketed? #'binding))
     (syntax-parse #'binding
       [(k:id v:id)
        (define tok (range-token #'op))
        #`(go:RangeStmt #:id (new-id!) #:for #f #:key (->ident* 'k) #:value (->ident* 'v) #:tok-pos #f #:tok '#,tok #:range #f #:x (->expr (go-expr x)) #:body (block body ...))]
       [(k:id)
        (define tok (range-token #'op))
        #`(go:RangeStmt #:id (new-id!) #:for #f #:key (->ident* 'k) #:value #f #:tok-pos #f #:tok '#,tok #:range #f #:x (->expr (go-expr x)) #:body (block body ...))]
       [()
        (define tok (range-token #'op))
        #`(go:RangeStmt #:id (new-id!) #:for #f #:key #f #:value #f #:tok-pos #f #:tok '#,tok #:range #f #:x (->expr (go-expr x)) #:body (block body ...))]
       [_ (raise-syntax-error 'for-range "expected [], [value], or [key value]" #'binding)])]
    [(_ x:expr body:expr ...+)
     #'(go:RangeStmt #:id (new-id!) #:for #f #:key #f #:value #f #:tok-pos #f #:tok go-token:ILLEGAL #:range #f #:x (->expr (go-expr x)) #:body (block body ...))]))

;; --- helpers for switch -------------------------------------------------
;; ---------------- helpers ----------------
(begin-for-syntax
  ;; one or many case expressions
  (define-syntax-class case-exprs
    #:attributes (vals)
    ;; Multiple case values are grouped with square brackets. Parenthesized
    ;; expressions remain single expressions, so (case (f x) ...) is safe.
    (pattern (~and whole (es:exprish ...+))
      #:when (square-bracketed? #'whole)
      #:with vals #'(list (->expr es.datum) ...))
    (pattern e:exprish
      #:with vals #'(list (->expr e.datum))))

  (define-syntax-class switch-clause
    #:attributes (node)
    #:datum-literals (case default)
    ;; case clause
    (pattern (case cx:case-exprs body:expr ...+)
      #:with node
      #'(go:CaseClause #:id (new-id!) #:case #f #:list cx.vals #:colon #f #:body (list (expr->stmt body) ...)))
    ;; default clause
    (pattern (default body:expr ...+)
      #:with node
      #'(go:CaseClause #:id (new-id!) #:case #f #:list #f #:colon #f #:body (list (expr->stmt body) ...))))

  ;; Nice error if someone writes bare [2 3] without `case`
  (define (ensure-clause c)
    (syntax-parse c
      #:datum-literals (case default)
      [(case _ ...)   #'#t]
      [(default _ ...) #'#t]
      [[_ . _]
       (raise-syntax-error 'switch
         "expected (case <exprs> ...) or (default ...); did you forget `case`?" c)]
      [_ (raise-syntax-error 'switch
           "expected switch clause: (case ...) or (default ...)" c)]))
)

(define-syntax (switch stx)
  (syntax-parse stx
    #:datum-literals (case default init)
    [(_ (init init-expr:expr) first:switch-clause rest:switch-clause ...)
     #'(go:SwitchStmt #:id (new-id!) #:switch #f #:init (expr->stmt init-expr) #:tag #f #:body (go:BlockStmt #:id (new-id!) #:lbrace #f #:list (list first.node rest.node ...) #:rbrace #f))]
    [(_ (init init-expr:expr) tag:expr cl:switch-clause ...+)
     #'(go:SwitchStmt #:id (new-id!) #:switch #f #:init (expr->stmt init-expr) #:tag (->expr (go-expr tag)) #:body (go:BlockStmt #:id (new-id!) #:lbrace #f #:list (list cl.node ...) #:rbrace #f))]
    [(_ first:switch-clause rest:switch-clause ...)
     #'(go:SwitchStmt #:id (new-id!) #:switch #f #:init #f #:tag #f #:body (go:BlockStmt #:id (new-id!) #:lbrace #f #:list (list first.node rest.node ...) #:rbrace #f))]
    [(_ tag:expr cl:switch-clause ...+)
     #'(go:SwitchStmt #:id (new-id!) #:switch #f #:init #f #:tag (->expr (go-expr tag)) #:body (go:BlockStmt #:id (new-id!) #:lbrace #f #:list (list cl.node ...) #:rbrace #f))]
    [(_ raw ...+)
     (begin
       (for ([c (in-list (syntax->list #'(raw ...)))]) (ensure-clause c))
       (raise-syntax-error 'switch
                           "expected (switch [(init stmt)] [tag] (case ...) ...)"
                           #'(raw ...)))]))

;; channel: dir is 'SEND|'RECV|both (default both)
;; channel type: chan T, chan<- T, <-chan T
(define (chan* value #:dir [dir 'both])
  (go:ChanType #:id (new-id!) #:begin #f #:arrow #f #:dir (case dir
      [(send) 'SEND]
      [(recv) 'RECV]
      [else   'BOTH]) #:value (->expr value)))

;; optional sugar
(define (chan value)       (chan* value #:dir 'both))
(define (chan-send value)  (chan* value #:dir 'send)) ; chan<- T
(define (chan-recv value)  (chan* value #:dir 'recv)) ; <-chan T
;; --------------- imports & packages ----------------
(define-syntax (import stx)
  (syntax-parse stx
    [(_ path:string)
     #`(go:ImportSpec #:id (new-id!) #:doc #f #:name #f #:path (go:BasicLit #:id (new-id!) #:value-pos #f #:kind 'STRING #:value (go-string #,(syntax-e #'path))) #:comment #f #:end-pos #f)]
    [(_ alias:id path:string)
     #:when (eq? (syntax-e #'alias) '_)
     #`(go:ImportSpec #:id (new-id!) #:doc #f #:name (->ident* "_") #:path (go:BasicLit #:id (new-id!) #:value-pos #f #:kind 'STRING #:value (go-string #,(syntax-e #'path))) #:comment #f #:end-pos #f)]
    [(_ alias:id path:string)
     #:when (eq? (syntax-e #'alias) 'dot)
     #`(go:ImportSpec #:id (new-id!) #:doc #f #:name (->ident* ".") #:path (go:BasicLit #:id (new-id!) #:value-pos #f #:kind 'STRING #:value (go-string #,(syntax-e #'path))) #:comment #f #:end-pos #f)]
    [(_ alias:id path:string)
     #`(go:ImportSpec #:id (new-id!) #:doc #f #:name (->ident* '#,(syntax-e #'alias)) #:path (go:BasicLit #:id (new-id!) #:value-pos #f #:kind 'STRING #:value (go-string #,(syntax-e #'path))) #:comment #f #:end-pos #f)]))





(define-syntax (package stx)
  (syntax-parse stx
    [(_ name:id elem:expr ...+)
     #'(let* ([nodes (list elem ...)]
              [imports (filter go:ImportSpec? nodes)]
              [decls (append
                      (for/list ([is imports]) (go:GenDecl #:id (new-id!) #:doc #f #:tok-pos #f #:tok go-token:IMPORT #:lparen #f #:specs (list is) #:rparen #f))
                      (filter (λ (n) (or (go:FuncDecl? n) (go:GenDecl? n))) nodes))])
         (file-with 'name decls imports))]))
;; ----- helpers used by var -----
(define (mk-valuespec idents type values)
  (go:ValueSpec #:id (new-id!) #:doc #f #:names (map ->ident* idents) #:type (and type (->expr type)) #:values (maybe-null (map ->expr (or values '()))) #:comment #f))

;; go/printer uses valid Lparen/Rparen positions to decide whether a GenDecl
;; with multiple specs is grouped. Gorack has no source token for this purely
;; syntactic choice, so reserve a synthetic file-relative position whenever
;; grouping is required.
(define (mk-gen-decl token specs)
  (define grouping-position
    (and (racket:> (length specs) 1) (ast-position "generated" 0)))
  (go:GenDecl #:id (new-id!) #:doc #f #:tok-pos #f #:tok token #:lparen grouping-position #:specs specs #:rparen grouping-position))

;; var declarations:
;;   (var x T)
;;   (var [x T] [y U] ...)
;;   (var #:specs spec ...)
(define-syntax (var stx)
  (syntax-parse stx
    ;; many: (var [x T] [y U] ...)
    [(_ [name:id type:expr] ...)
     (with-syntax ([(spec ...)
                    (for/list ([n (in-list (syntax->list #'(name ...)))]
                               [t (in-list (syntax->list #'(type ...)))])
                      #`(mk-valuespec (list '#,(syntax-e n)) #,t '()))])
       #`(->decl (mk-gen-decl go-token:VAR (list spec ...))))]

    ;; single: (var x T)
    [(_ name:id type:expr)
     #`(->decl (mk-gen-decl go-token:VAR
                            (list (mk-valuespec (list '#,(syntax-e #'name)) type '()))))]

    ;; explicit pass-through: (var #:specs spec ...)
    [(_ #:specs spec:expr ...)
     #`(->decl (mk-gen-decl go-token:VAR (list spec ...)))]))

(define-syntax (var= stx)
  (syntax-parse stx
    ;; Many triplets: (var= [x T vx] [y U vy] ...)
    [(_ [name:id type:expr val:expr] ...)
     (with-syntax ([(spec ...)
                    (for/list ([n (in-list (syntax->list #'(name ...)))]
                               [t (in-list (syntax->list #'(type ...)))]
                               [v (in-list (syntax->list #'(val  ...)))])
                      #`(mk-valuespec (list '#,(syntax-e n)) #,t (list #,v)))])
       #`(->decl (mk-gen-decl go-token:VAR (list spec ...))))]

    ;; Many names share RHS (no explicit type): (var= [x y z] v1 v2 v3)
    [(_ [names:id ...] v:expr ...+)
     (with-syntax ([(ids ...)
                    (for/list ([n (in-list (syntax->list #'(names ...)))])
                      #`'#,(syntax-e n))])
       #`(->decl (mk-gen-decl go-token:VAR
                              (list (mk-valuespec (list ids ...) #f (list v ...))))))]

    ;; Single typed with init (bracket form): (var= [x T] vx)
    [(_ [name:id type:expr] val:expr)
     #`(->decl (mk-gen-decl go-token:VAR
                            (list (mk-valuespec (list '#,(syntax-e #'name)) type (list val)))))]

    ;; Single typed with init (bare form): (var= x T vx)
    [(_ name:id type:expr val:expr)
     #`(->decl (mk-gen-decl go-token:VAR
                            (list (mk-valuespec (list '#,(syntax-e #'name)) type (list val)))))]

    ;; Pass-through: (var= #:specs spec ...)
    [(_ #:specs spec:expr ...)
     #`(->decl (mk-gen-decl go-token:VAR (list spec ...)))]))

;; const= — always with initializers
(define-syntax (const= stx)
  (syntax-parse stx
    ;; many triplets: (const= [A T vA] [B U vB] ...)
    [(_ [name:id type:expr val:expr] ...)
     (with-syntax ([(spec ...)
                    (for/list ([n (in-list (syntax->list #'(name ...)))]
                               [t (in-list (syntax->list #'(type ...)))]
                               [v (in-list (syntax->list #'(val  ...)))])
                      #`(mk-valuespec (list '#,(syntax-e n)) #,t (list #,v)))])
       #`(mk-gen-decl go-token:CONST (list spec ...)))]

    ;; many names share RHS (no explicit type): (const= [A B C] v1 v2 v3)
    [(_ [names:id ...] v:expr ...+)
     (with-syntax ([(ids ...)
                    (for/list ([n (in-list (syntax->list #'(names ...)))])
                      #`'#,(syntax-e n))])
       #`(mk-gen-decl go-token:CONST
                      (list (mk-valuespec (list ids ...) #f (list v ...)))))]

    ;; single typed: (const= A T v)
    [(_ name:id type:expr val:expr)
     #`(mk-gen-decl go-token:CONST
                    (list (mk-valuespec (list '#,(syntax-e #'name)) type (list val))))]

    ;; single untyped: (const= A v)
    [(_ name:id val:expr)
     #`(mk-gen-decl go-token:CONST
                    (list (mk-valuespec (list '#,(syntax-e #'name)) #f (list val))))]

    ;; pass-through: (const= #:specs spec ...)
    [(_ #:specs spec:expr ...)
     #`(mk-gen-decl go-token:CONST (list spec ...))]))


(define (mk-typedecl name assign? ty)
  (go:TypeSpec #:id (new-id!) #:doc #f #:name (->ident* name) #:type-params #f #:assign (and assign? (ast-position "generated" 0)) #:type (->expr ty) #:comment #f))

(define-syntax (type stx)
  (syntax-parse stx
    [(_ name:id ty:expr)
     #'(mk-gen-decl go-token:TYPE (list (mk-typedecl 'name #f ty)))]))

(define-syntax (type-alias stx)
  (syntax-parse stx
    [(_ name:id ty:expr)
     #'(mk-gen-decl go-token:TYPE (list (mk-typedecl 'name #t ty)))]))


;; --------------- functions ----------------


(define (params->fieldlist ps)
  (go:FieldList #:id (new-id!) #:opening #f #:list (for/list ([p ps])
      (match p
        [(list nm ty) (go:Field #:id (new-id!) #:doc #f #:names (list (->ident* nm)) #:type (->expr ty) #:tag #f #:comment #f)]
        [_ (error 'params "param must be (name type), got ~v" p)])) #:closing #f))

(define (results->fieldlist rs)
  (and (pair? rs)
       (go:FieldList #:id (new-id!) #:opening #f #:list (for/list ([r rs])
           (match r
             [(list nm ty) (go:Field #:id (new-id!) #:doc #f #:names (list (->ident* nm)) #:type (->expr ty) #:tag #f #:comment #f)]
             [ty           (go:Field #:id (new-id!) #:doc #f #:names '() #:type (->expr ty) #:tag #f #:comment #f)])) #:closing #f)))


;; ---------- compile-time helpers ----------


;; Functions and methods use one headed signature form.
(define-syntax (defn stx)
  (syntax-parse stx
    [(_ (rname:id rty:typeish) name:id sig:go-signature body:expr ...+)
     #`(let* ([recv (go:FieldList #:id (new-id!) #:opening #f #:list (list (go:Field #:id (new-id!) #:doc #f #:names (list (->ident* 'rname)) #:type (->expr rty.datum) #:tag #f #:comment #f)) #:closing #f)]
              [ft (go:FuncType #:id (new-id!) #:func #f #:type-params #f #:params (params->fieldlist sig.params) #:results (results->fieldlist sig.results))]
              [blk (go:BlockStmt #:id (new-id!) #:lbrace #f #:list (map expr->stmt (list body ...)) #:rbrace #f)])
         (go:FuncDecl #:id (new-id!) #:doc #f #:recv recv #:name (->ident* 'name) #:type ft #:body blk))]
    [(_ name:id sig:go-signature body:expr ...+)
     #`(let* ([ft (go:FuncType #:id (new-id!) #:func #f #:type-params #f #:params (params->fieldlist sig.params) #:results (results->fieldlist sig.results))]
              [blk (go:BlockStmt #:id (new-id!) #:lbrace #f #:list (map expr->stmt (list body ...)) #:rbrace #f)])
         (go:FuncDecl #:id (new-id!) #:doc #f #:recv #f #:name (->ident* 'name) #:type ft #:body blk))]))

(define-syntax-rule (ptr T) (go:StarExpr #:id (new-id!) #:star #f #:x (->expr T)))
;; the above rule is used for pointer receivers

;; ---------- interface ----------
(begin-for-syntax
  (define-syntax-class iface-elem
    #:attributes (field)
    #:datum-literals (method embed ->)
    (pattern (method nm:id sig:go-signature)
      #:with field
      #`(go:Field #:id (new-id!) #:doc #f #:names (list (->ident* 'nm)) #:type (go:FuncType #:id (new-id!) #:func #f #:type-params #f #:params (params->fieldlist sig.params) #:results (results->fieldlist sig.results)) #:tag #f #:comment #f))
    (pattern (embed ty:expr)
      #:with field
      #'(go:Field #:id (new-id!) #:doc #f #:names '() #:type (->expr ty) #:tag #f #:comment #f))
    (pattern ty:expr
      #:with field
      #'(go:Field #:id (new-id!) #:doc #f #:names '() #:type (->expr ty) #:tag #f #:comment #f))))

(define-syntax (gorack-interface stx)
  (syntax-parse stx
    [(_ elem:iface-elem ...+)
     #'(go:InterfaceType #:id (new-id!) #:interface #f #:methods (go:FieldList #:id (new-id!) #:opening #f #:list (list elem.field ...) #:closing #f) #:incomplete #f)]))


(define-syntax (gorack-break stx)
  (syntax-parse stx
    [(_) #'(go:BranchStmt #:id (new-id!) #:tok-pos #f #:tok go-token:BREAK #:label #f)]
    [(_ name:id) #'(go:BranchStmt #:id (new-id!) #:tok-pos #f #:tok go-token:BREAK #:label (->ident* 'name))]))

(define-syntax (gorack-continue stx)
  (syntax-parse stx
    [(_) #'(go:BranchStmt #:id (new-id!) #:tok-pos #f #:tok go-token:CONTINUE #:label #f)]
    [(_ name:id) #'(go:BranchStmt #:id (new-id!) #:tok-pos #f #:tok go-token:CONTINUE #:label (->ident* 'name))]))

(define-syntax (gorack-go stx)
  (syntax-parse stx
    [(_ e:expr) #'(go:GoStmt #:id (new-id!) #:go #f #:call (->expr (go-expr e)))]))

(define-syntax (gorack-defer stx)
  (syntax-parse stx
    [(_ e:expr) #'(go:DeferStmt #:id (new-id!) #:defer #f #:call (->expr (go-expr e)))]))

(define-syntax (send stx)
  (syntax-parse stx
    [(_ ch:expr v:expr) #'(go:SendStmt #:id (new-id!) #:chan (->expr (go-expr ch)) #:arrow #f #:value (->expr (go-expr v)))]))

(define-syntax (inc stx)
  (syntax-parse stx
    [(_ x:expr) #'(go:IncDecStmt #:id (new-id!) #:x (->expr (go-expr x)) #:tok-pos #f #:tok go-token:INC)]))

(define-syntax (dec stx)
  (syntax-parse stx
    [(_ x:expr) #'(go:IncDecStmt #:id (new-id!) #:x (->expr (go-expr x)) #:tok-pos #f #:tok go-token:DEC)]))

(define-syntax (label stx)
  (syntax-parse stx
    [(_ name:id body:expr)
     #'(go:LabeledStmt #:id (new-id!) #:label (->ident* 'name) #:colon #f #:stmt (expr->stmt body))]))

(define-syntax (goto stx)
  (syntax-parse stx
    [(_ name:id) #'(go:BranchStmt #:id (new-id!) #:tok-pos #f #:tok go-token:GOTO #:label (->ident* 'name))]))

(define-syntax (fallthrough stx)
  (syntax-parse stx
    [(_) #'(go:BranchStmt #:id (new-id!) #:tok-pos #f #:tok go-token:FALLTHROUGH #:label #f)]))

;; --- decl: wrap a GenDecl as a statement so it can appear inside blocks ---
;; use only with (var ...)/(const ...)/(type ...) etc.
(define-syntax (decl stx)
  (syntax-parse stx
    [(_ g:expr) #'(go:DeclStmt #:id (new-id!) #:decl g)]))

;; Select communication clauses use ordinary headed send, receive, and
;; assignment forms.
(define-syntax (select stx)
  (define (receive-assignment-token op-stx)
    (case (syntax-e op-stx)
      [(:=) 'DEFINE]
      [(=) 'ASSIGN]
      [else (raise-syntax-error 'select "expected := or =" op-stx)]))
  (syntax-parse stx
    #:datum-literals (case default send <-)
    [(_ raw-clause ...+)
     (define clauses
       (for/list ([clause (in-list (syntax->list #'(raw-clause ...)))])
         (syntax-parse clause
           #:datum-literals (case default send <-)
           [(case (send ch:expr value:expr) body:expr ...+)
            #'(go:CommClause #:id (new-id!) #:case #f #:comm (go:SendStmt #:id (new-id!) #:chan (->expr (go-expr ch)) #:arrow #f #:value (->expr (go-expr value))) #:colon #f #:body (list (expr->stmt body) ...))]
           [(case (<- ch:expr) body:expr ...+)
            #'(go:CommClause #:id (new-id!) #:case #f #:comm (go:ExprStmt #:id (new-id!) #:x (unary go-token:ARROW (go-expr ch))) #:colon #f #:body (list (expr->stmt body) ...))]
           [(case (op:id x:id (<- ch:expr)) body:expr ...+)
            (define tok (receive-assignment-token #'op))
            #`(go:CommClause #:id (new-id!) #:case #f #:comm (go:AssignStmt #:id (new-id!) #:lhs (list (->ident* 'x)) #:tok-pos #f #:tok '#,tok #:rhs (list (unary go-token:ARROW (go-expr ch)))) #:colon #f #:body (list (expr->stmt body) ...))]
           [(case (op:id binding (<- ch:expr)) body:expr ...+)
            #:when (square-bracketed? #'binding)
            (syntax-parse #'binding
              [(x:id ok:id)
               (define tok (receive-assignment-token #'op))
               #`(go:CommClause #:id (new-id!) #:case #f #:comm (go:AssignStmt #:id (new-id!) #:lhs (list (->ident* 'x) (->ident* 'ok)) #:tok-pos #f #:tok '#,tok #:rhs (list (unary go-token:ARROW (go-expr ch)))) #:colon #f #:body (list (expr->stmt body) ...))]
              [_ (raise-syntax-error 'select "receive binding must be [value ok]" #'binding)])]
           [(default body:expr ...+)
            #'(go:CommClause #:id (new-id!) #:case #f #:comm #f #:colon #f #:body (list (expr->stmt body) ...))])))
     (with-syntax ([(clause-node ...) clauses])
       #'(go:SelectStmt #:id (new-id!) #:select #f #:body (go:BlockStmt #:id (new-id!) #:lbrace #f #:list (list clause-node ...) #:rbrace #f)))]))

;; Build the type-switch guard as either an assignment or expression.
(define-for-syntax (ts-guard id tok subject-stx)
  (define rhs
    #`(go:TypeAssertExpr #:id (new-id!) #:x (->expr (go-expr #,subject-stx)) #:lparen #f #:type #f #:rparen #f))
  (cond
    [id   #`(go:AssignStmt #:id (new-id!) #:lhs (list (->ident* '#,id)) #:tok-pos #f #:tok '#,tok #:rhs (list #,rhs))]
    [else #`(go:ExprStmt #:id (new-id!) #:x #,rhs)]))

(define-syntax (type-switch stx)
  (define (guard-token op-stx)
    (case (syntax-e op-stx)
      [(:=) 'DEFINE]
      [(=) 'ASSIGN]
      [else (raise-syntax-error 'type-switch "expected := or =" op-stx)]))
  (define (build-clauses raw)
    (for/list ([clause (in-list (syntax->list raw))])
      (syntax-parse clause
        #:datum-literals (case default)
        [(case grouped body:expr ...+)
         #:when (square-bracketed? #'grouped)
         (syntax-parse #'grouped
           [(tys:expr ...+)
            #'(go:CaseClause #:id (new-id!) #:case #f #:list (list (->expr tys) ...) #:colon #f #:body (list (expr->stmt body) ...))]
           [_ (raise-syntax-error 'type-switch "expected one or more types" #'grouped)])]
        [(case ty:expr body:expr ...+)
         #'(go:CaseClause #:id (new-id!) #:case #f #:list (list (->expr ty)) #:colon #f #:body (list (expr->stmt body) ...))]
        [(default body:expr ...+)
         #'(go:CaseClause #:id (new-id!) #:case #f #:list #f #:colon #f #:body (list (expr->stmt body) ...))])))
  (syntax-parse stx
    [(_ (op:id id:id subject:expr) raw-clause ...+)
     #:when (memq (syntax-e #'op) '(:= =))
     (define clauses (build-clauses #'(raw-clause ...)))
     (define token (guard-token #'op))
     (with-syntax ([(clause-node ...) clauses]
                   [guard (ts-guard (syntax-e #'id) token #'subject)])
       #'(go:TypeSwitchStmt #:id (new-id!) #:switch #f #:init #f #:assign guard #:body (go:BlockStmt #:id (new-id!) #:lbrace #f #:list (list clause-node ...) #:rbrace #f)))]
    [(_ subject:expr raw-clause ...+)
     (define clauses (build-clauses #'(raw-clause ...)))
     (with-syntax ([(clause-node ...) clauses]
                   [guard (ts-guard #f #f #'subject)])
       #'(go:TypeSwitchStmt #:id (new-id!) #:switch #f #:init #f #:assign guard #:body (go:BlockStmt #:id (new-id!) #:lbrace #f #:list (list clause-node ...) #:rbrace #f)))]))


;; Function literals use the same signature form as defn.

(define-syntax (fn stx)
  (syntax-parse stx
    [(_ sig:go-signature body:expr ...+)
     #'(go:FuncLit #:id (new-id!)
                  #:type (go:FuncType #:id (new-id!) #:func #f #:type-params #f #:params (params->fieldlist sig.params) #:results (results->fieldlist sig.results))
                  #:body (go:BlockStmt #:id (new-id!) #:lbrace #f #:list (map expr->stmt (list body ...)) #:rbrace #f))]))


;; `nil` receives an ID in the consuming module's allocation sequence.
;; A module-level node would survive `reset-ids!` and collide with the first
;; node constructed by every Gorack program.
(define-syntax nil
  (syntax-id-rules ()
    [nil (->ident* "nil")]))



;; Make unbound identifiers become go:Ident values.

;; Build a selector chain: a.b.c  =>  ((a).b).c as go:SelectorExpr nodes
(begin-for-syntax
  (define (split-dotted sym)
    (define parts (string-split (symbol->string sym) "."))
    (and (racket:> (length parts) 1) (map string->symbol parts)))

  (define (mk-selector-chain parts)
    (define first (car parts))
    (for/fold ([acc #`(->ident* '#,first)])
              ([fld (in-list (cdr parts))])
      #`(go:SelectorExpr #:id (new-id!) #:x #,acc #:sel (->ident* '#,fld)))))


(define-syntax (gorack-#%top stx)
  (syntax-parse stx
    [(_ . id:id)
     (define sym   (syntax-e #'id))
     (define parts (split-dotted sym))
     (if parts
         ;; r.x.y  → chain of go:SelectorExpr with fresh ids
         (mk-selector-chain parts)
         ;; plain id → go:Ident
         #`(->ident* '#,sym))]))


;; Simple module-begin that just evaluates forms (they should produce a go:File or decls)

(define-syntax (gorack-#%module-begin stx)
  (syntax-parse stx
    [(_ e:expr ...)
     #'(#%plain-module-begin
        (reset-ids!)
        (define last (begin e ...))
        (define out-file
          (cond
            [(go:File? last) last]
            [(and (list? last)
                  (andmap (λ (d) (or (go:GenDecl? d) (go:FuncDecl? d))) last))
             (file-with 'main last '())]
            [else
              (let* ([main-ty (go:FuncType #:id (new-id!) #:func #f #:type-params #f #:params #f #:results #f)]
                      [main-fn (go:FuncDecl #:id (new-id!) #:doc #f #:recv #f #:name (->ident* 'main) #:type main-ty #:body (block last))])
                (file-with 'main (list main-fn) '()))]
            
            ))
        (write-go-kernel-json out-file)
        (void))]))
