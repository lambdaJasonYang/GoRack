#lang gorack

(package main
  (import "fmt")

  (defn main
    (-> () ())
    (:= x 10)
    (:= y 32)
    (:= z (+ x y))
    (fmt.Println "sum:" z)))
