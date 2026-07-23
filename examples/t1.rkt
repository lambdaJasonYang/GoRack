#lang gorack

(package hi
  (defn add
    (-> ([x int] [y int]) (int))
    (return (+ x y))))
