#lang gorack

(package methods
  (import "fmt")

  ;; receiver r of type *T
    (defn (r (ptr T)) move ((dx int) (dy int))
      (assign += [p.x] [dx])
      (assign += [p.y] [dy])
      (return))
    (defn (r T) MethodName () (return))
    (defn (r (ptr T)) SetValue ((dx int)) (return))
(defn (r T) normalize () -> (Point)
  (block
    (:= [mag] [(call r.magnitude)])
    (return
      (composite Point
                 (kv x (/ r.x mag))
                 (kv y (/ r.y mag))))))

)