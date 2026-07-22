#lang gorack
(package hi
  (import "fmt")
  (defn add ([x int] [y int]) -> (int)
    (return (+ x y))))
