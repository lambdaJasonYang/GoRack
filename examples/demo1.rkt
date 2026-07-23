#lang gorack

(package demo
  (import "fmt")

  (type Person
    (struct
      (Name string)
      (Age int)
      (Note string (tag (json note #:omitempty)))))

  (const= AppName "gorack-demo")

  (defn add
    (-> ([x int] [y int]) (int))
    (return (+ x y)))

  (defn main
    (-> () ())
    (:= p
      (composite Person
        (kv Name "Ada")
        (kv Age 37)))
    (:= total (add 40 2))
    (fmt.Printf "%s: %s is %d; total=%d\n"
      AppName p.Name p.Age total)))
