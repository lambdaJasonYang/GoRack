#lang gorack
(package demo
  (import "fmt")

  (type Person
    (struct_
      (Name string)
      (Age  int)
      (Note string (tag (json note #:omitempty)))))

  (const= AppName "gorack-demo")

  (defn add ([x int] [y int]) -> (int)
    (return (+ x y)))

  (defn main ()
    (:= p (composite Person
              [(kv Name "Ada") (kv Age 37)]))
    (:= total (call add 40 2))
    (call fmt.Printf "%s: %s is %d; total=%d\n"
          AppName (sel p Name) (sel p Age) total)))
