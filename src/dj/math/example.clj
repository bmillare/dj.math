(ns dj.math.example
  (:require [dj.math :as dm]
            [dj.math.matrix :as dmm]
            [dj.math.parser :as dmp]))

(defn Q
  "computes reflection matrix given column vector x and e is a column
  vector that is part of a basis of orthonormal vectors, usually
  [1,0...0]^T"
  [x e]
  (let [alpha (dmm/vnorm x)
        u (dm/- x (dm/* alpha e))
        v (dm/d u (dmm/vnorm u))
        I (dmm/identity-m (count x))]
    ;; need to zero out below element 0,0
    ;; actually we can replace column 1 with alpha, and zeroes

    ;; next we will need a way to obtain minors and piece them back together

    ;; a way to merge them back would be needed too
    (dm/- I (dm/* 2 (dm/* v (dmm/t v))))))

(let [A (dmm/v [[12 -51 4]
                [6 167 -68]
                [-4 24 -41]])]
  (dm/* (Q (first (dmm/col-seq A))
           (dmm/t (dmm/v [[1 0 0]])))
        A))



