#lang gorack

(package methods
  (defn main
    (-> () ())
    (:= x 1)
    (if #t
      (= x 2))
    (index arr 2)
    (slice-expr s 1 3)
    (slice-expr s 1 3 5)
    (type-assert x T)
    (array 3 int)
    (slice int)
    (map string int)
    (! ok)
    (and a b)
    (>= 7 5)
    (block (:= y 9) (= _ y))
    (if condy
      (= x 3)
      (= x 4))
    (for keepGoing
      (= keepGoing #f))
    (for (:= i 0) (< i 3) (+= i 1)
      (= x i))
    (return)))
