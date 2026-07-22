#lang gorack
(package main
  (import "fmt")

  (defn main ()
    (assign := x 10)
    (assign := y 32)
    (assign := z (+ x y))
    (call fmt.Println "sum:" z)))
