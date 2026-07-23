#lang gorack

(package broadexample
  (import "context")
  (import "fmt")
  (import "io")

  (type Point
    (struct
      (x float64)
      (y float64)))

  (type Reader
    (interface
      (method Read (-> ([p (slice byte)]) (int error)))
      (embed io.Reader)))

  (defn useContext
    (-> ([ctx context.Context]) ())
    (= _ ctx))

  (defn divmod
    (-> ([a int] [b int]) (int int))
    (return (/ a b) (% a b)))

  (defn chooseAny
    (-> ([a int] [b int]) (any))
    (= _ b)
    (return a))

  ;; Surface form names remain ordinary Go identifiers in operand position.
  (defn identityIndex
    (-> ([index int]) (int))
    (return index))

  (defn (p (ptr Point)) move
    (-> ([dx float64] [dy float64]) ())
    (+= p.x dx)
    (+= p.y dy))

  (defn main
    (-> () ())
    (:= index (identityIndex 0))
    (+= index 1)
    (if (> index 0)
      (fmt.Println "index:" index))

    (:= xs (composite (slice int) 1 2 3))
    (for-range (:= [index value] xs)
      (fmt.Println index value))

    (:= ch (make (chan int)))
    (go (fmt.Println "async"))
    (select
      (case (:= [value ok] (<- ch))
        (fmt.Println value ok))
      (default
        (fmt.Println "no value")))

    ;; Multiple assignment targets can receive one multi-valued expression.
    (:= [received receiveOK] (<- ch))
    (= _ received)
    (= _ receiveOK)

    (:= anyValue (any 1))
    (type-switch (:= value anyValue)
      (case int
        (fmt.Println "int" value))
      (case [string bool]
        (fmt.Println "string or bool"))
      (default
        (fmt.Println "other")))

    ;; A call-shaped subject must not be mistaken for a binding clause.
    (type-switch (chooseAny 1 2)
      (case int
        (fmt.Println "chosen int"))
      (default
        (fmt.Println "chosen other")))

    (switch (init (:= n (len xs))) n
      (case 0 (fmt.Println "empty"))
      (default (fmt.Println "items" n)))

    (if (init (:= n (len xs))) (> n 0)
      (fmt.Println "non-empty"))

    ;; A begin-only branch must remain an if without an empty else block.
    (:= ready #t)
    (if ready
      (begin
        (= ready #f)
        (fmt.Println "ready")))

    ;; Condition loops remain unambiguous with several body statements.
    (:= running #f)
    (for running
      (= running #f)
      (fmt.Println "loop")
      (break))

    (for (forever)
      (break))
    (return)))
