#lang gorack

(package methods
  (defn (r (ptr T)) move
    (-> ([dx int] [dy int]) ())
    (+= r.x dx)
    (+= r.y dy)
    (return))

  (defn (r T) MethodName
    (-> () ())
    (return))

  (defn (r (ptr T)) SetValue
    (-> ([dx int]) ())
    (return))

  (defn (r T) normalize
    (-> () (Point))
    (:= mag (r.magnitude))
    (return
      (composite Point
        (kv x (/ r.x mag))
        (kv y (/ r.y mag))))))
