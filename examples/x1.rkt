#lang racket
(require "../gorack/lang/main.rkt"
         "../gorack/go-kernel/wire.rkt")

(define prog
  (package hi
    (import "fmt")
    (defn add ([x int] [y int] ) -> (int)
        (return (+ x y)))
    (defn noResult ()
      (return))
    (defn square ([x int]) -> (int)
      (return (* x x)))
    (defn divmod ([a int] [b int]) -> (int int)
      (return (/ a b)
              (% a b)))
    (defn doNothing ()
      (return))
    ; (defn getAnswer () -> (int)
    ;     (return 42))    
    ; (defn greet ((name strin))
    ;     (call printlna "Hello" name))
    ; (defn mixTypes ((flag bool) (cnt int)) -> (str)
    ;     (if_ flag
    ;          (return (mkstr "True and count is") cnt)
    ;          (return (mkstr "False and count is") cnt)))
    (defn useSquare ((n int)) -> (int)
        (return (call square n)))
    (defn (r (ptr 'T)) move ((dx int) (dy int))
      (assign += [(sel p x)] [dx])
      (assign += [(sel p y)] [dy])
      (return))
    (defn (r 'T) MethodName () (return))
    (defn (r (ptr 'T)) SetValue ((dx int)) (return))
    (defn (r 'T) normalize () -> (Point)
            (block
              (assign := [mag] [(call (sel p magnitude))])
              (return (composite Point
                                (list (kv x (/ (sel p x) mag))
                                      (kv y (/ (sel p y) mag)))))))

    ;; (optional) a couple of top-level decls
    (defn main ()
      (:= x 1)
      (if_ #t #t)
      (index arr 2)
      (slice-expr s 1 3)
      (slice-expr s 1 3 5)
      (type-assert x T)
      (array 3 int)
      (array '... byte)
      (slice int)
      (map_ str int)
      (struct_ x int)
      (struct_ (x y ) int)
      (composite Point (list (kv x 1) (kv y 2)) )
      (! ok)
      (&& a b)
      (>= 7 5)
      (return (+ 2 3))
      (block (:= x 9) (return 9))
      (if_ condy  
        (return 3)
        (return 4)
      )
      (for_ keepGoing
        (return 0)
      )
      (for_ (:= i 0)
        (binary "LSS" i 3)
        (+= i 1)
        (return i)
      )

(type MyStruct
  (struct_
    ; (FieldName    string ())
    (AnotherField int    (tag (json another_field #:omitempty)))))
      ;; short decls + compound assigns + inc/dec
    (for_ (= x 1)
              (< i 3)
              (+= [i f] [3 5])
              (continue* Top))
    (for_ (return 1))
    (for-range := [k v] xs 
        (return k))
    (for-range := [k] xs 
        (return k))
    (for-range := [] xs 
        (return k))
    (switch bleh
      (case 1 (return h))
      (case ((== x y)) (return b))
      (case (2 3 4) (return c))
      (default (return)))
    (switch 
      (case 1 (return h))
      (case ((== y z)) (return b))
      (default (return)))
    (chan* int)
    (chan* str #:dir 'send)
    (chan* bool   #:dir 'recv)
    (var n int )
    (var= n int 10)
    (var= [x y z] 1 2 3)
    (var= [u v] int 1 2)  
    (const= a 4)
    (interface_
        (method Read ([p (slice byte)]) -> (int error))
        (embed io.Reader))

    )))

;; write JSON for your Go side to round-trip to .go
(write-go-ast-json-file "x1.wire.json" prog)
