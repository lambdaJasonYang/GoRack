#lang gorack

(package decls
  (var x int)
  (var [y string] [z int])
  (var= [a int 1] [b string "bee"])
  (var= [m n k] 10 20 30)
  (const= PI 3.14)
  (const= [Sun Mon Tue] 0 1 2)

  (defn main
    (-> () ())
    (decl (var t int))
    (= t 42)
    (= _ t)
    (return)))
