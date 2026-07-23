#lang gorack

(package methods
  (defn useSquare
    (-> ([n int]) (int))
    (return (square n)))

  (defn add
    (-> ([x int] [y int]) (int))
    (return (+ x y)))

  (defn noResult
    (-> () ())
    (return))

  (defn square
    (-> ([x int]) (int))
    (return (* x x))))
