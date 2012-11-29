(ns dj.math.example
  (:require [dj.math :as dm]
            [dj.math.matrix :as dmm]
            [dj.math.parser :as dmp]))



(defn qr-decomp [A]
  (if (= 1
         (count A))
    {:R nil
     :Q nil}
    (let [x (first (dmm/col-seq A))
          I (dmm/identity-m (count x))
          ;; computes reflection matrix given column vector x and e is a
          ;; column vector that is part of a basis of orthonormal vectors,
          ;; usually [1,0...0]^T
          e (dmm/t (dmm/v [[1 0 0]]))
          alpha (dmm/vnorm x)
          u (dm/- x (dm/* alpha e))
          v (dm/d u (dmm/vnorm u))
          Q (dm/- I (dm/* 2 (dm/* v (dmm/t v))))
          QA (dmm/assoc-minor (dm/* Q
                                    A)
                              [0 0]
                              (dm/* (first (dmm/col-seq I))
                                    alpha))]
      ;; this will crash, needs to be put in a loop that can detect when
      ;; done
      (let [A' (dmm/minor QA [1 1])
            Q' (qr-decomp A')
            Qk (dmm/assoc-minor I
                                [1 1]
                                Q')]
        {:R (dm/* Qk QA)
         :Q (dm/* (dmm/t Q) (dmm/t Qk))}))))

(qr-decomp (dmm/v [[12 -51 4]
                   [6 167 -68]
                   [-4 24 -41]]))