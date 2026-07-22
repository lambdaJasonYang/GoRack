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

;; export the language kernel bindings
(provide
  (rename-out [gorack-#%module-begin #%module-begin]   ; pass-through begin
              [gorack-#%top        #%top]
              [gorack-#%top        #%top-interaction]
              [gorack-#%datum      #%datum]
              ) ; optional, nice for REPL
  #%app
  (all-defined-out)
  (all-from-out "../go-kernel/generated/tokens.rkt")
  )
;; Emit the versioned Go-kernel graph envelope.
(define (write-go-kernel-json n)
  (write-go-ast-json (current-output-port) n))

(define (write-go-kernel-json-file path n)
  (write-go-ast-json-file path n))


;; === put this NEAR THE TOP of lang/main.rkt, before switch/switch* ===
(begin-for-syntax
   ;; Bare id → (->ident* 'id); anything else passes through.
  (define-syntax-class exprish
    #:attributes (datum)
    (pattern id:id
      #:with datum #`(->ident* '#,(syntax-e #'id)))
    (pattern e:expr
      #:with datum #'e)))
;; ---------- compile-time helpers ----------
(begin-for-syntax
  ;; An identifier like `int`/`error` → (->ident* 'int), arbitrary expr passes through.
  (define-syntax-class typeish
    #:attributes (datum)
    (pattern id:id
      #:with datum #`(->ident* '#,(syntax-e #'id)))
    ;; Optional: accept string type names as a future surface convenience.
    ; (pattern s:string
    ;   #:with datum #`(->ident* #,(syntax-e #'s)))
    (pattern e:expr
      #:with datum #'e))

  ;; Accept [x T] or (x T). T is id / "string" / expr.
  (define-syntax-class param-pair
    #:attributes (name type datum)
    (pattern [nm:id ty:typeish]
      #:with name  #'nm
      #:with type  #'ty.datum
      #:with datum #`(list '#,(syntax-e #'nm) ty.datum))
    (pattern (nm:id ty:typeish)
      #:with name  #'nm
      #:with type  #'ty.datum
      #:with datum #`(list '#,(syntax-e #'nm) ty.datum)))

  ;; Results can be named or unnamed; same typeish handling.
  (define-syntax-class result-item
    #:attributes (datum)
    (pattern [nm:id ty:typeish]
      #:with datum #`(list '#,(syntax-e #'nm) ty.datum))
    (pattern (nm:id ty:typeish)
      #:with datum #`(list '#,(syntax-e #'nm) ty.datum))
    (pattern ty:typeish
      #:with datum #'ty.datum))
)



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

; (define (->ident* s) (go:Ident (new-id!) #f (if (symbol? s) (symbol->string s) s)))
(define (->ident* s)
  (cond
    [(symbol? s) (go:Ident #:id (new-id!) #:name-pos #f #:name (symbol->string s))]
    [(string? s) (go:Ident #:id (new-id!) #:name-pos #f #:name s)]
    [else (error '->ident*
            (format "expected symbol/string, got ~v" s))]))


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
    [(symbol? v)   (->ident* v)]
    [(string? v)   (->ident* v)]
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

(define (ident v) (->ident* v))

;;------------functions you would actually call in your gorack lang to construct go types
(define (mkstr s)   (go:BasicLit #:id (new-id!) #:value-pos #f #:value-end #f #:kind 'STRING #:value (go-string s)))
(define str "string") ;;racket `string` conflicts with string
(define (mkint n)   (go:BasicLit #:id (new-id!) #:value-pos #f #:value-end #f #:kind 'INT #:value (number->string n)))
(define (mkfloat x) (go:BasicLit #:id (new-id!) #:value-pos #f #:value-end #f #:kind 'FLOAT #:value (number->string x)))
(define (mkbool b)  (->ident* (if b 'true 'false)))
(define (mkrune c)  (go:BasicLit #:id (new-id!) #:value-pos #f #:value-end #f #:kind 'CHAR #:value (format "'~a'" c)))
(define (mkimag s)  (go:BasicLit #:id (new-id!) #:value-pos #f #:value-end #f #:kind 'IMAG #:value s))

;; Preserve an official Go literal's spelling exactly. The surface reader
;; deliberately turns ordinary Racket literals into Go AST nodes.
;; Consuming KIND and VALUE in a macro prevents that useful datum behaviour
;; from wrapping an already-constructed BasicLit a second time.
(define-syntax (go-literal stx)
  (syntax-parse stx
    [(_ kind:id value:string)
     (define kind-symbol (syntax-e #'kind))
     (define literal-value (syntax-e #'value))
     #`(go:BasicLit #:id (new-id!) #:value-pos #f #:value-end #f #:kind '#,kind-symbol #:value '#,literal-value)]))
;;-----------------------------------------------------------------------------
(define-syntax (sel stx)
  (syntax-parse stx
    [(_ x:expr field:id)
     #`(go:SelectorExpr #:id (new-id!) #:x (->expr x) #:sel (->ident* '#,(syntax-e #'field)))]))
; (define-syntax (sel stx)
;   (syntax-parse stx
;     [(_ x:expr field:id)
;      #'(go:SelectorExpr (new-id!) (->expr x) (->ident* 'field))]))

;; Pass-through module-begin: expand user forms as a normal Racket module.
; (define-syntax (dsl-#%module-begin stx)
;   (syntax-parse stx
;     [(_ form ...)

;      #'(#%plain-module-begin
;         form ...)]))

;;helper for call  
(define (spread xs) (list 'spread xs))

(define (call f . args)
  ;; support optional variadic "spread" sentinel in the last position
  (define spread?
    (and (pair? args)
         (pair? (last args))
         (eq? (car (last args)) 'spread)))

  ;; if you ever use (spread x), keep x as the last arg
  (define args*
    (if spread?
        (append (drop-right args 1) (list (cadr (last args))))
        args))

  (go:CallExpr #:id (new-id!) #:fun (->expr f) #:lparen #f #:args (map ->expr args*) #:ellipsis (and spread? (ast-position "generated" 0)) #:rparen #f))
;; use like: (call f a b (spread xs))

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
(define (map_ key value)  (go:MapType #:id (new-id!) #:map #f #:key (->expr key) #:value (->expr value)))

;; runtime (constructor) — renamed to avoid clashing with macro
;; -------- compile-time helper for struct fields --------
;; runtime helper: make a Go raw string literal with backticks
;; Backtick (raw) Go string for struct tags
(define (mkrawtag s)
  (go:BasicLit #:id (new-id!) #:value-pos #f #:value-end #f #:kind 'STRING #:value (format "`~a`" s)))

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
(define-syntax (struct_ stx)
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


; (define (composite ty elts)
;   (go:CompositeLit
;    (new-id!) (->expr ty) #f
;    (for/list ([e elts])
;      (cond [(go:KeyValueExpr? e) e]
;            [(and (pair? e) (eq? (car e) 'kv)) (apply kv (cdr e))]
;            [else (->expr e)]))
;    #f #f))

; (define (composite ty elts)
;   (go:CompositeLit
;    #f (->expr ty) #f
;    (for/list ([e elts])
;      (cond [(go:KeyValueExpr? e) e]
;            [(and (pair? e) (eq? (car e) 'kv)) (apply kv (cdr e))]
;            [else (->expr e)]))
;    #f #f))


  ;; --------------- operators ---------------

(define (unary op x) (go:UnaryExpr #:id (new-id!) #:op-pos #f #:op op #:x (->expr x)))
(define (binary op x y) (go:BinaryExpr #:id (new-id!) #:x (->expr x) #:op-pos #f #:op op #:y (->expr y)))

;; Unary
(define-syntax-rule (! x)  (unary go-token:NOT x))
(define-syntax-rule (~ x)  (unary go-token:TILDE x)) ; for constraints
(define-syntax-rule (u* x) (go:StarExpr #:id (new-id!) #:star #f #:x (->expr x)))
(define-syntax-rule (u& x) (unary go-token:AND x))
(define-syntax-rule (<- x) (unary go-token:ARROW x))

;; Binary
(define-syntax-rule (+ a b)   (binary go-token:ADD a b))
(define-syntax-rule (- a b)   (binary go-token:SUB a b))
(define-syntax-rule (* a b)   (binary go-token:MUL a b))
(define-syntax-rule (/ a b)   (binary go-token:QUO a b))
(define-syntax-rule (% a b)   (binary go-token:REM a b))
(define-syntax-rule (+/ a b)   (binary go-token:OR a b))
(define-syntax-rule (& a b)   (binary go-token:AND a b))
(define-syntax-rule (^ a b)   (binary go-token:XOR a b))
(define-syntax-rule (<< a b)  (binary go-token:SHL a b))
(define-syntax-rule (>> a b)  (binary go-token:SHR a b))
(define-syntax-rule (&^ a b)  (binary go-token:AND_NOT a b))
(define-syntax-rule (== a b)  (binary go-token:EQL a b))
(define-syntax-rule (!= a b)  (binary go-token:NEQ a b))
(define-syntax-rule (< a b)   (binary go-token:LSS a b))
(define-syntax-rule (<= a b)  (binary go-token:LEQ a b))
(define-syntax-rule (> a b)   (binary go-token:GTR a b))
(define-syntax-rule (>= a b)  (binary go-token:GEQ a b))
(define-syntax-rule (&& a b)  (binary go-token:LAND a b))
(define-syntax-rule (++/ a b)  (binary go-token:LOR a b))

;; assignment: (assign := [lhs ...] [rhs ...]) or (assign += [lhs] [rhs])
; (define (assign-token s)
;   (case s
;     [(::=) 'DEFINE] [(=) 'ASSIGN]
;     [(+=) 'ADD_ASSIGN] [(-=) 'SUB_ASSIGN] [(*=) 'MUL_ASSIGN] [(/=) 'QUO_ASSIGN]
;     [(%=) 'REM_ASSIGN] [(&=) 'AND_ASSIGN] [(OR=) 'OR_ASSIGN] [(^=) 'XOR_ASSIGN]
;     [(<<=) 'SHL_ASSIGN] [(>>=) 'SHR_ASSIGN] [(&^=) 'AND_NOT_ASSIGN]
;     [else (error 'assign "unknown assignment token ~a" s)]))

;; assignment: (assign := [lhs ...] [rhs ...]) etc.
;; assignment: (assign := [lhs ...] [rhs ...]) etc.
;; --- assign: accept single or list forms --------------------------------
(define-syntax (assign stx)
  (syntax-parse stx
    ;; canonical list form (what you already support)
    [(_ tok:id [lhs:expr ...] [rhs:expr ...])
     (define tok-sym (syntax-e #'tok))
     (define go-tok
       (case tok-sym
         [(:=)   'DEFINE]
         [(=)    'ASSIGN]
         [(+=)   'ADD_ASSIGN]
         [(-=)   'SUB_ASSIGN]
         [(*=)   'MUL_ASSIGN]
         [(/=)   'QUO_ASSIGN]
         [(%=)   'REM_ASSIGN]
         [(&=)   'AND_ASSIGN]
         [(OR=)  'OR_ASSIGN]
         [(^=)   'XOR_ASSIGN]
         [(<<=)  'SHL_ASSIGN]
         [(>>=)  'SHR_ASSIGN]
         [(&^=)  'AND_NOT_ASSIGN]
         [else (raise-syntax-error 'assign "unknown assignment token" #'tok)]))
     #`(go:AssignStmt #:id #f #:lhs (list (->expr lhs) ...) #:tok-pos #f #:tok '#,go-tok #:rhs (list (->expr rhs) ...))]

    ;; NEW: single lhs / single rhs without brackets
    [(_ tok:id lhs:expr rhs:expr)
     #'(assign tok [lhs] [rhs])]))
;; Generate macros like (:= x v) or (:= [x y] [v w]) that delegate to assign
(define-syntax (define-assign-op stx)
  (syntax-parse stx
    [(_ name:id)
     #`(define-syntax (name stx)
         (syntax-parse stx
           ;; forward everything after the operator to (assign name ...)
           [(_ . rest) #'(assign name . rest)]))]))

;; Instantiate for all assignment tokens you support
(define-assign-op :=)
(define-assign-op =)
(define-assign-op +=)
(define-assign-op -=)
(define-assign-op *=)
(define-assign-op /=)
(define-assign-op %=)
(define-assign-op &=)
(define-assign-op OR=)
(define-assign-op ^=)
(define-assign-op <<=)
(define-assign-op >>=)
(define-assign-op &^=)

;; --------------- statements ----------------

(define (return . xs)
  (go:ReturnStmt #:id (new-id!) #:return #f #:results (map ->expr xs)))

(define (block . body)
  (go:BlockStmt #:id (new-id!) #:lbrace #f #:list (map expr->stmt body) #:rbrace #f))

; if_ : 
;   (if_ cond then...)                               ; no else
;   (if_ cond (begin then...) (begin else...))       ; explicit blocks
;   (if_ cond (begin then...) else...)               ; else as trailing seq
;; replace your current if_ with this
(define-syntax (if_ stx)
  (syntax-parse stx
    #:datum-literals (begin)

    ;; both branches explicitly wrapped
    [(_ cond:expr (begin thn:expr ...) (begin els:expr ...))
     #'(go:IfStmt #:id (new-id!) #:if #f #:init #f #:cond (->expr cond) #:body (block thn ...) #:else (block els ...))]

    ;; then wrapped; else is a trailing sequence
    [(_ cond:expr (begin thn:expr ...) els:expr ...)
     #'(go:IfStmt #:id (new-id!) #:if #f #:init #f #:cond (->expr cond) #:body (block thn ...) #:else (block els ...))]

    ;; NEW: single then + single else (no begin on either)
    [(_ cond:expr thn:expr els:expr)
     #'(go:IfStmt #:id (new-id!) #:if #f #:init #f #:cond (->expr cond) #:body (block thn) #:else (block els))]

    ;; then-only form
    [(_ cond:expr thn:expr ...)
     #'(go:IfStmt #:id (new-id!) #:if #f #:init #f #:cond (->expr cond) #:body (block thn ...) #:else #f)]))

;; Turn an expression-ish thing into a statement


(define-syntax (for_ stx)
  (syntax-parse stx
    ;; for init; cond; post { body }
    [(_ init:expr cond:expr post:expr body:expr ...+)
     #'(go:ForStmt #:id (new-id!) #:for #f #:init (expr->stmt init) #:cond (->expr cond) #:post (expr->stmt post) #:body (block body ...))]

    ;; for cond { body }
    [(_ cond:expr body:expr ...+)
     #'(go:ForStmt #:id (new-id!) #:for #f #:init #f #:cond (->expr cond) #:post #f #:body (block body ...))]

    ;; for { body }
    [(_ body:expr ...+)
     #'(go:ForStmt #:id (new-id!) #:for #f #:init #f #:cond #f #:post #f #:body (block body ...))]))

;; for-range:
;;   (for-range [k v] := x body...)
;;   (for-range [k]   := x body...)
;;   (for-range []    := x body...)
(define-syntax (for-range stx)
  (syntax-parse stx
    [(_ tok:id [k:id v:id] x:expr body:expr ...+)
     (define tok-s (syntax-e #'tok))
     (define go-tok
       (case tok-s
         [(:=) 'DEFINE]
         [(=)  'ASSIGN]
         [else (raise-syntax-error 'for-range "expected := or =" #'tok)]))
     #`(let-syntax ([k (λ (_stx) #'(->ident* 'k))]
                    [v (λ (_stx) #'(->ident* 'v))])
         (go:RangeStmt #:id (new-id!) #:for #f #:key (->ident* 'k) #:value (->ident* 'v) #:tok-pos #f #:tok '#,go-tok #:range #f #:x (->expr x) #:body (block body ...)))]

    [(_ tok:id [k:id] x:expr body:expr ...+)
     (define tok-s (syntax-e #'tok))
     (define go-tok
       (case tok-s
         [(:=) 'DEFINE]
         [(=)  'ASSIGN]
         [else (raise-syntax-error 'for-range "expected := or =" #'tok)]))
     #`(let-syntax ([k (λ (_stx) #'(->ident* 'k))])
         (go:RangeStmt #:id (new-id!) #:for #f #:key (->ident* 'k) #:value #f #:tok-pos #f #:tok '#,go-tok #:range #f #:x (->expr x) #:body (block body ...)))]

    [(_ tok:id [] x:expr body:expr ...+)
     (define tok-s (syntax-e #'tok))
     (define go-tok
       (case tok-s
         [(:=) 'DEFINE]
         [(=)  'ASSIGN]
         [else (raise-syntax-error 'for-range "expected := or =" #'tok)]))
     #`(go:RangeStmt #:id (new-id!) #:for #f #:key #f #:value #f #:tok-pos #f #:tok '#,go-tok #:range #f #:x (->expr x) #:body (block body ...))]))

;; --- helpers for switch -------------------------------------------------
;; ---------------- helpers ----------------
(begin-for-syntax
  ;; one or many case expressions
  (define-syntax-class case-exprs
    #:attributes (vals)
    ;; many with brackets: (case [e1 e2 ...] ...)
    (pattern [es:exprish ...+]
      #:with vals #'(list (->expr es.datum) ...))
    ;; single value/expression: (case e ...)
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
      #'(go:CaseClause #:id (new-id!) #:case #f #:list '() #:colon #f #:body (list (expr->stmt body) ...))))

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
    #:datum-literals (case default)
    ;; no-tag
    [(_ first:switch-clause rest:switch-clause ...+)
     #'(go:SwitchStmt #:id (new-id!) #:switch #f #:init #f #:tag #f #:body (go:BlockStmt #:id (new-id!) #:lbrace #f #:list (list first.node rest.node ...) #:rbrace #f))]
    ;; tagged
    [(_ tag:expr cl:switch-clause ...+)
     #'(go:SwitchStmt #:id (new-id!) #:switch #f #:init #f #:tag (->expr tag) #:body (go:BlockStmt #:id (new-id!) #:lbrace #f #:list (list cl.node ...) #:rbrace #f))]
    ;; catch mistakes early
    [(_ raw ...+)
     (begin (for ([c (in-list (syntax->list #'(raw ...)))]) (ensure-clause c))
            (raise-syntax-error 'switch
              "could not parse; expected (switch [tag-expr] (case ...) ...)"
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
     #`(go:ImportSpec #:id (new-id!) #:doc #f #:name #f #:path (go:BasicLit #:id (new-id!) #:value-pos #f #:value-end #f #:kind 'STRING #:value (go-string #,(syntax-e #'path))) #:comment #f #:end-pos #f)]
    [(_ alias:id path:string)
     #:when (eq? (syntax-e #'alias) '_)
     #`(go:ImportSpec #:id (new-id!) #:doc #f #:name (->ident* "_") #:path (go:BasicLit #:id (new-id!) #:value-pos #f #:value-end #f #:kind 'STRING #:value (go-string #,(syntax-e #'path))) #:comment #f #:end-pos #f)]
    [(_ alias:id path:string)
     #:when (eq? (syntax-e #'alias) 'dot)
     #`(go:ImportSpec #:id (new-id!) #:doc #f #:name (->ident* ".") #:path (go:BasicLit #:id (new-id!) #:value-pos #f #:value-end #f #:kind 'STRING #:value (go-string #,(syntax-e #'path))) #:comment #f #:end-pos #f)]
    [(_ alias:id path:string)
     #`(go:ImportSpec #:id (new-id!) #:doc #f #:name (->ident* '#,(syntax-e #'alias)) #:path (go:BasicLit #:id (new-id!) #:value-pos #f #:value-end #f #:kind 'STRING #:value (go-string #,(syntax-e #'path))) #:comment #f #:end-pos #f)]))

; (define-syntax (import stx)
;   (syntax-parse stx
;     [(_ path:string)
;      #'(go:ImportSpec (new-id!) #f #f
;                        (go:BasicLit (new-id!) #f 'STRING (go-string path)) #f #f)]

;     ;; blank import: (import _ "pkg")
;     [(_ alias:id path:string)
;      #:when (eq? (syntax-e #'alias) '_)
;      #'(go:ImportSpec (new-id!) #f (->ident* "_")
;                        (go:BasicLit (new-id!) #f 'STRING (go-string path)) #f #f)]

;     ;; dot import: (import dot "pkg")
;     [(_ alias:id path:string)
;      #:when (eq? (syntax-e #'alias) 'dot)
;      #'(go:ImportSpec (new-id!) #f (->ident* ".")
;                        (go:BasicLit (new-id!) #f 'STRING (go-string path)) #f #f)]

;     ;; named import: (import Printf "fmt")
;     [(_ alias:id path:string)
;      #`(go:ImportSpec (new-id!) #f (->ident* '#,(syntax-e #'alias))
;                        (go:BasicLit (new-id!) #f 'STRING (go-string path)) #f #f)]))

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

;; var:
;;   (var (decl ...))                          ; already-built specs
;;   (var x T)                                 ; single name/type
;;   (var x T := v)                            ; single name/type + init
;;   (var [x T] [y T] ...)                     ; many, each with its own type
;;   (var [x T] [y U] ... := v w ...)          ; many names, shared init exprs (no explicit type)
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
    [(_ name:id = ty:expr)
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


;; defn — switch to p.datum
(define-syntax (defn stx)
  (syntax-parse stx
    #:datum-literals (->)

    ;; method, WITH results  (try first)
    [(_ (rname:id rty:expr) name:id (p:param-pair ...) -> (ret:expr ...) body:expr ...+)
     #`(let* ([recv (go:FieldList #:id (new-id!) #:opening #f #:list (list (go:Field #:id (new-id!) #:doc #f #:names (list (->ident* 'rname)) #:type (->expr rty) #:tag #f #:comment #f)) #:closing #f)]
              [ft   (go:FuncType #:id (new-id!) #:func #f #:type-params #f #:params (params->fieldlist (list p.datum ...)) #:results (results->fieldlist (list ret ...)))]
              [blk  (go:BlockStmt #:id (new-id!) #:lbrace #f #:list (map expr->stmt (list body ...)) #:rbrace #f)])
         (go:FuncDecl #:id (new-id!) #:doc #f #:recv recv #:name (->ident* 'name) #:type ft #:body blk))]

    ;; plain function, WITH results  (try second)
    [(_ name:id (p:param-pair ...) -> (ret:expr ...) body:expr ...+)
     #`(let* ([ft  (go:FuncType #:id (new-id!) #:func #f #:type-params #f #:params (params->fieldlist (list p.datum ...)) #:results (results->fieldlist (list ret ...)))]
              [blk (go:BlockStmt #:id (new-id!) #:lbrace #f #:list (map expr->stmt (list body ...)) #:rbrace #f)])
         (go:FuncDecl #:id (new-id!) #:doc #f #:recv #f #:name (->ident* 'name) #:type ft #:body blk))]

    ;; method, NO results
    [(_ (rname:id rty:expr) name:id (p:param-pair ...) body:expr ...+)
     #`(let* ([recv (go:FieldList #:id (new-id!) #:opening #f #:list (list (go:Field #:id (new-id!) #:doc #f #:names (list (->ident* 'rname)) #:type (->expr rty) #:tag #f #:comment #f)) #:closing #f)]
              [ft   (go:FuncType #:id (new-id!) #:func #f #:type-params #f #:params (params->fieldlist (list p.datum ...)) #:results #f)]
              [blk  (go:BlockStmt #:id (new-id!) #:lbrace #f #:list (map expr->stmt (list body ...)) #:rbrace #f)])
         (go:FuncDecl #:id (new-id!) #:doc #f #:recv recv #:name (->ident* 'name) #:type ft #:body blk))]

    ;; plain function, NO results
    [(_ name:id (p:param-pair ...) body:expr ...+)
     #`(let* ([ft  (go:FuncType #:id (new-id!) #:func #f #:type-params #f #:params (params->fieldlist (list p.datum ...)) #:results #f)]
              [blk (go:BlockStmt #:id (new-id!) #:lbrace #f #:list (map expr->stmt (list body ...)) #:rbrace #f)])
         (go:FuncDecl #:id (new-id!) #:doc #f #:recv #f #:name (->ident* 'name) #:type ft #:body blk))]))

;; method/embed sugar that returns *data* for (interface ...)
(define-syntax (method stx)
  (syntax-parse stx
    #:datum-literals (->)
    [(_ nm:id (p:param-pair ...) -> (r:result-item ...))
     #'(list 'method 'nm (list p.datum ...) '-> (list r.datum ...))]
    [(_ nm:id (p:param-pair ...))
     #'(list 'method 'nm (list p.datum ...) '-> (list))]))

(define-syntax (embed stx)
  (syntax-parse stx
    [(_ ty:expr) #'(list 'embed ty)]))
(define-syntax-rule (ptr T) (go:StarExpr #:id (new-id!) #:star #f #:x (->expr T)))
;; the above rule is used for pointer receivers

;; ---------- interface (macro) ----------
;; Put this near your other macros (after param-pair/result-item are defined)

(begin-for-syntax
  ;; Turn interface items into go:Field construction
  (define-syntax-class iface-elem
    #:attributes (field)
    #:datum-literals (method embed ->)

    ;; method with results
    (pattern (method nm:id (p:param-pair ...) -> (r:result-item ...))
      #:with field
      #`(go:Field #:id (new-id!) #:doc #f #:names (list (->ident* 'nm)) #:type (go:FuncType #:id (new-id!) #:func #f #:type-params #f #:params (params->fieldlist (list p.datum ...)) #:results (results->fieldlist (list r.datum ...))) #:tag #f #:comment #f))

    ;; method with NO results
    (pattern (method nm:id (p:param-pair ...))
      #:with field
      #`(go:Field #:id (new-id!) #:doc #f #:names (list (->ident* 'nm)) #:type (go:FuncType #:id (new-id!) #:func #f #:type-params #f #:params (params->fieldlist (list p.datum ...)) #:results #f) #:tag #f #:comment #f))

    ;; embed form
    (pattern (embed ty:expr)
      #:with field
      #`(go:Field #:id (new-id!) #:doc #f #:names '() #:type (->expr ty) #:tag #f #:comment #f))

    ;; bare type (shorthand for embed)
    (pattern ty:expr
      #:with field
      #`(go:Field #:id (new-id!) #:doc #f #:names '() #:type (->expr ty) #:tag #f #:comment #f))))

(define-syntax (interface_ stx)
  (syntax-parse stx
    [(_ elem:iface-elem ...+)
     #`(go:InterfaceType #:id (new-id!) #:interface #f #:methods (go:FieldList #:id (new-id!) #:opening #f #:list (list elem.field ...) #:closing #f) #:incomplete #f)]))


(define (break)    (go:BranchStmt #:id (new-id!) #:tok-pos #f #:tok go-token:BREAK #:label #f))
(define (continue) (go:BranchStmt #:id (new-id!) #:tok-pos #f #:tok go-token:CONTINUE #:label #f))


;; --- go / defer ---
;; replace the earlier versions with these

(define-syntax (go_ stx)
  (syntax-parse stx
    [(_ e:expr) #'(go:GoStmt #:id (new-id!) #:go #f #:call (->expr e))]))

(define-syntax (defer_ stx)
  (syntax-parse stx
    [(_ e:expr) #'(go:DeferStmt #:id (new-id!) #:defer #f #:call (->expr e))]))

;; --- send statement: ch <- v ---
(define-syntax (send stx)
  (syntax-parse stx
    [(_ ch:expr v:expr) #'(go:SendStmt #:id (new-id!) #:chan (->expr ch) #:arrow #f #:value (->expr v))]))

;; --- ++ / -- ---
(define-syntax (inc stx)
  (syntax-parse stx
    [(_ x:expr) #'(go:IncDecStmt #:id (new-id!) #:x (->expr x) #:tok-pos #f #:tok go-token:INC)]))

(define-syntax (dec stx)
  (syntax-parse stx
    [(_ x:expr) #'(go:IncDecStmt #:id (new-id!) #:x (->expr x) #:tok-pos #f #:tok go-token:DEC)]))

;; --- labels, break/continue with optional label, and goto ---
(define-syntax (label stx)
  (syntax-parse stx
    [(_ name:id body:expr)
     #'(go:LabeledStmt #:id (new-id!) #:label (->ident* 'name) #:colon #f #:stmt (expr->stmt body))]))

(define-syntax (break* stx)
  (syntax-parse stx
    [(_ )        #'(go:BranchStmt #:id (new-id!) #:tok-pos #f #:tok go-token:BREAK #:label #f)]
    [(_ name:id) #'(go:BranchStmt #:id (new-id!) #:tok-pos #f #:tok go-token:BREAK #:label (->ident* 'name))]))

(define-syntax (continue* stx)
  (syntax-parse stx
    [(_ )        #'(go:BranchStmt #:id (new-id!) #:tok-pos #f #:tok go-token:CONTINUE #:label #f)]
    [(_ name:id) #'(go:BranchStmt #:id (new-id!) #:tok-pos #f #:tok go-token:CONTINUE #:label (->ident* 'name))]))

(define-syntax (goto stx)
  (syntax-parse stx
    [(_ name:id) #'(go:BranchStmt #:id (new-id!) #:tok-pos #f #:tok go-token:GOTO #:label (->ident* 'name))]))

; ------------- End UNIT 3 ------------------

;; --- fallthrough (for switch cases) ---
(define-syntax (fallthrough stx)
  (syntax-parse stx
    [(_)
     #'(go:BranchStmt #:id (new-id!) #:tok-pos #f #:tok go-token:FALLTHROUGH #:label #f)]))

;; --- decl: wrap a GenDecl as a statement so it can appear inside blocks ---
;; use only with (var ...)/(const ...)/(type ...) etc.
(define-syntax (decl stx)
  (syntax-parse stx
    [(_ g:expr) #'(go:DeclStmt #:id (new-id!) #:decl g)]))

;; --- select ---
;;   (select
;;     (case (send ch v)           body ...+)
;;     (case (recv [x] := ch)      body ...+)
;;     (case (recv ch)             body ...+) ; discard recv value
;;     (default                    body ...+))
(define-syntax (select stx)
  (define (tok->sym s)
    (case s [(:=) 'DEFINE] [(=) 'ASSIGN]
          [else (raise-syntax-error 'select "expected := or =" #f)]))
  (syntax-parse stx
    #:datum-literals (case default recv send := =)

    [(_ raw-clause ...+)
     (define clauses
       (for/list ([c (in-list (syntax->list #'(raw-clause ...)))])
         (syntax-parse c
           ;; send: case ch <- v
           [(case (send ch:expr v:expr) body:expr ...+)
            #`(go:CommClause #:id (new-id!) #:case #f #:comm (go:SendStmt #:id (new-id!) #:chan (->expr ch) #:arrow #f #:value (->expr v)) #:colon #f #:body (list (expr->stmt body) ...))]

           ;; recv with assign: case [x] := ch   or   [x] = ch
           [(case (recv [x:id] tok:id ch:expr) body:expr ...+)
            #`(go:CommClause #:id (new-id!) #:case #f #:comm (go:AssignStmt #:id (new-id!)
                               #:lhs (list (->ident* 'x))
                               #:tok-pos #f
                               #:tok '#,(tok->sym (syntax-e #'tok))
                               #:rhs (list (unary go-token:ARROW ch))) #:colon #f #:body (list (expr->stmt body) ...))]

           ;; recv, discard value: case (recv ch)
           [(case (recv ch:expr) body:expr ...+)
            #`(go:CommClause #:id (new-id!) #:case #f #:comm (go:ExprStmt #:id (new-id!) #:x (unary go-token:ARROW ch)) #:colon #f #:body (list (expr->stmt body) ...))]
            ;; inside your existing (select ...) transformer, add this clause:
            [(case (recv [x:id ok:id] tok:id ch:expr) body ...+)
            #`(go:CommClause #:id (new-id!) #:case #f #:comm (go:AssignStmt #:id (new-id!)
                                #:lhs (list (->ident* 'x) (->ident* 'ok))
                                #:tok-pos #f
                                #:tok '#,(tok->sym (syntax-e #'tok))
                                #:rhs (list (unary go-token:ARROW ch))) #:colon #f #:body (list (expr->stmt body) ...))]

           ;; default
           [(default body:expr ...+)
            #'(go:CommClause #:id (new-id!) #:case #f #:comm #f #:colon #f #:body (list (expr->stmt body) ...))])))

     (with-syntax ([(cl ...) clauses])
       #'(go:SelectStmt #:id (new-id!) #:select #f #:body (go:BlockStmt #:id (new-id!) #:lbrace #f #:list (list cl ...) #:rbrace #f)))]))
; ------------------------------------------


;; ========= if with init =========
(define-syntax (if* stx)
  (syntax-parse stx
    #:datum-literals (begin)

    ;; init; cond; then {..} else {..}
    [(_ init:expr cond:expr (begin thn:expr ...) (begin els:expr ...))
     #'(go:IfStmt #:id (new-id!) #:if #f #:init (expr->stmt init) #:cond (->expr cond) #:body (block thn ...) #:else (block els ...))]

    ;; init; cond; then {..} else ... (as trailing seq)
    [(_ init:expr cond:expr (begin thn:expr ...) els:expr ...)
     #'(go:IfStmt #:id (new-id!) #:if #f #:init (expr->stmt init) #:cond (->expr cond) #:body (block thn ...) #:else (block els ...))]

    ;; init; cond; then-only
    [(_ init:expr cond:expr thn:expr ...)
     #'(go:IfStmt #:id (new-id!) #:if #f #:init (expr->stmt init) #:cond (->expr cond) #:body (block thn ...) #:else #f)]))


(define-syntax (switch* stx)
  (syntax-parse stx
    #:datum-literals (case default)

    ;; init + tag
    [(_ init:expr tag:expr raw-clause ...+)
     (define case-asts
       (for/list ([c (in-list (syntax->list #'(raw-clause ...)))])
         (syntax-parse c
           [(case (es:exprish ...) body:expr ...+)
            #'(go:CaseClause #:id (new-id!) #:case #f #:list (list (->expr es.datum) ...) #:colon #f #:body (list (expr->stmt body) ...))]
           [(default body:expr ...+)
            #'(go:CaseClause #:id (new-id!) #:case #f #:list '() #:colon #f #:body (list (expr->stmt body) ...))])))
     (with-syntax ([(cl ...) case-asts])
       #'(go:SwitchStmt #:id (new-id!) #:switch #f #:init (expr->stmt init) #:tag (->expr tag) #:body (go:BlockStmt #:id (new-id!) #:lbrace #f #:list (list cl ...) #:rbrace #f)))]

    ;; init only (no tag)
    [(_ init:expr raw-clause ...+)
     (define case-asts
       (for/list ([c (in-list (syntax->list #'(raw-clause ...)))])
         (syntax-parse c
           [(case (es:exprish ...) body:expr ...+)
            #'(go:CaseClause #:id (new-id!) #:case #f #:list (list (->expr es.datum) ...) #:colon #f #:body (list (expr->stmt body) ...))]
           [(default body:expr ...+)
            #'(go:CaseClause #:id (new-id!) #:case #f #:list '() #:colon #f #:body (list (expr->stmt body) ...))])))
     (with-syntax ([(cl ...) case-asts])
       #'(go:SwitchStmt #:id (new-id!) #:switch #f #:init (expr->stmt init) #:tag #f #:body (go:BlockStmt #:id (new-id!) #:lbrace #f #:list (list cl ...) #:rbrace #f)))]))

;; ========= switch with init (switch*) =========
; (define-syntax (switch* stx)
;   (syntax-parse stx
;     #:datum-literals (case default)

;     ;; init + tag
;     [(_ init:expr tag:expr raw-clause ...+)
;      (define case-asts
;        (for/list ([c (in-list (syntax->list #'(raw-clause ...)))])
;          (syntax-parse c
;            [(case (es:expr ...) body:expr ...+)
;             #'(go:CaseClause (new-id!) #f
;                               (list (->expr es) ...)
;                               #f
;                               (list (expr->stmt body) ...))]
;            [(default body:expr ...+)
;             #'(go:CaseClause (new-id!) #f
;                               '()
;                               #f
;                               (list (expr->stmt body) ...))])))
;      (with-syntax ([(cl ...) case-asts])
;        #'(go:SwitchStmt (new-id!) #f (expr->stmt init) (->expr tag)
;            (go:BlockStmt (new-id!) #f (list cl ...) #f)))]

;     ;; init only (no tag)
;     [(_ init:expr raw-clause ...+)
;      (define case-asts
;        (for/list ([c (in-list (syntax->list #'(raw-clause ...)))])
;          (syntax-parse c
;            [(case (es:expr ...) body:expr ...+)
;             #'(go:CaseClause (new-id!) #f
;                               (list (->expr es) ...)
;                               #f
;                               (list (expr->stmt body) ...))]
;            [(default body:expr ...+)
;             #'(go:CaseClause (new-id!) #f
;                               '()
;                               #f
;                               (list (expr->stmt body) ...))])))
;      (with-syntax ([(cl ...) case-asts])
;        #'(go:SwitchStmt (new-id!) #f (expr->stmt init) #f
;            (go:BlockStmt (new-id!) #f (list cl ...) #f)))]))


; ;; helper: build the guard stmt
; (define-for-syntax (ts-guard id tok expr-stx)
;   (define rhs #`(go:TypeAssertExpr #f (->expr #,expr-stx) #f #f))
;   (cond
;     [id   #`(go:AssignStmt #f (list (->ident* '#,id)) #f '#,tok (list #,rhs))]
;     [else #`(go:ExprStmt #f #,rhs)]))

;; helper: build the guard stmt
;; helper: 5-arg go:TypeAssertExpr, produce either Assign or Expr guard
(define-for-syntax (ts-guard id tok subject-stx)
  (define rhs
    #`(go:TypeAssertExpr #:id (new-id!) #:x (->expr #,subject-stx) #:lparen #f #:type #f #:rparen #f))
  (cond
    [id   #`(go:AssignStmt #:id (new-id!) #:lhs (list (->ident* '#,id)) #:tok-pos #f #:tok '#,tok #:rhs (list #,rhs))]
    [else #`(go:ExprStmt #:id (new-id!) #:x #,rhs)]))

(define-syntax (type-switch stx)
  (define (tok->sym s)
    (case s [(:=) 'DEFINE] [(=) 'ASSIGN]
          [else (raise-syntax-error 'type-switch "expected := or =")]))
  (syntax-parse stx
    #:datum-literals (case default := =)

    ;; switch x := e.(type) { ... }
    [(_ [id:id tok:id] subject:expr raw-clause ...+)
     (define clauses
       (for/list ([c (in-list (syntax->list #'(raw-clause ...)))])
         (syntax-parse c
           [(case (tys:expr ...) body:expr ...+)
            #'(go:CaseClause #:id (new-id!) #:case #f #:list (list (->expr tys) ...) #:colon #f #:body (list (expr->stmt body) ...))]
           [(default body:expr ...+)
            #'(go:CaseClause #:id (new-id!) #:case #f #:list '() #:colon #f #:body (list (expr->stmt body) ...))])))
     (with-syntax ([(cl ...) clauses]
                   [g (ts-guard (syntax-e #'id) (tok->sym (syntax-e #'tok)) #'subject)])
       #'(go:TypeSwitchStmt #:id (new-id!) #:switch #f #:init #f #:assign g #:body (go:BlockStmt #:id (new-id!) #:lbrace #f #:list (list cl ...) #:rbrace #f)))]

    ;; switch e.(type) { ... }
    [(_ subject:expr raw-clause ...+)
     (define clauses
       (for/list ([c (in-list (syntax->list #'(raw-clause ...)))])
         (syntax-parse c
           [(case (tys:expr ...) body:expr ...+)
            #'(go:CaseClause #:id (new-id!) #:case #f #:list (list (->expr tys) ...) #:colon #f #:body (list (expr->stmt body) ...))]
           [(default body:expr ...+)
            #'(go:CaseClause #:id (new-id!) #:case #f #:list '() #:colon #f #:body (list (expr->stmt body) ...))])))
     (with-syntax ([(cl ...) clauses]
                   [g (ts-guard #f #f #'subject)])
       #'(go:TypeSwitchStmt #:id (new-id!) #:switch #f #:init #f #:assign g #:body (go:BlockStmt #:id (new-id!) #:lbrace #f #:list (list cl ...) #:rbrace #f)))]))



;; allow function literals to appear anywhere expressions can
;; (add this once next to your existing ->expr)
;; (define (->expr v) (cond [(or ... (go:FuncLit? v)) v] ...))

(define-syntax (fn stx)
  (syntax-parse stx
    #:datum-literals (->)

    ;; WITH results — must come first so `->` doesn’t get eaten by body:expr ...
    [(_ (p:param-pair ...) -> (ret:expr ...) body:expr ...+)
     #'(go:FuncLit #:id (new-id!) #:type (go:FuncType #:id (new-id!) #:func #f #:type-params #f #:params (params->fieldlist (list p.datum ...)) #:results (results->fieldlist (list ret ...))) #:body (go:BlockStmt #:id (new-id!) #:lbrace #f #:list (map expr->stmt (list body ...)) #:rbrace #f))]

    ;; NO results
    [(_ (p:param-pair ...) body:expr ...+)
     #'(go:FuncLit #:id (new-id!) #:type (go:FuncType #:id (new-id!) #:func #f #:type-params #f #:params (params->fieldlist (list p.datum ...)) #:results #f) #:body (go:BlockStmt #:id (new-id!) #:lbrace #f #:list (map expr->stmt (list body ...)) #:rbrace #f))]))


;; `nil` must receive an ID in the consuming module's allocation sequence.
;; A module-level node would survive `reset-ids!` and collide with the first
;; node constructed by every Gorack program.
(define-syntax nil
  (syntax-id-rules ()
    [nil (->ident* "nil")]))
(define (rune c) (go:BasicLit #:id (new-id!) #:value-pos #f #:value-end #f #:kind 'CHAR #:value (format "'~a'" c))) ; tweak escaping as needed
(define (imag s) (go:BasicLit #:id (new-id!) #:value-pos #f #:value-end #f #:kind 'IMAG #:value s))                 ; e.g., "10i"



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
; (define-syntax (gorack-#%top stx)
;   (syntax-parse stx
;     [(_ . id:id)
;      #`(->ident* '#,(syntax-e #'id))]))


;; Simple module-begin that just evaluates forms (they should produce a go:File or decls)
; (define-syntax (gorack-#%module-begin stx)
;   (syntax-parse stx
;     [(_ e:expr ... ) #'(#%module-begin e ...)]))

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
