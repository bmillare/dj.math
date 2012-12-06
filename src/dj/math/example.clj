(ns dj.math.example
  (:require [dj.math :as dm]
            [dj.math.matrix :as dmm]
            [dj.math.parser :as dmp]))

(defn qr-decomp-step
  [A I I-size Q R]
  (let [x (first (dmm/col-seq A))
        x-size (count x)
        idx (- I-size x-size)]
    (if (= 1 x-size)
      {:Q Q
       :R (dmm/assoc-minor R
                           [idx idx]
                           x)}
      (let [
            ;; computes reflection matrix given column vector x and e is a
            ;; column vector that is part of a basis of orthonormal vectors,
            ;; usually [1,0...0]^T
            e (dmm/t (dmm/v [(vec (concat [1]
                                          (repeat (dec x-size)
                                                  0)))]))
            ;; the sign of alpha should be opposite of kth coordinate of x, since
            ;; we want u to be as small as possible
            ;; you can use Math/copySign or in cuda copysignf
            alpha (dm/copy-sign (dmm/vnorm x)
                                (first (seq x)))
            I-step (dmm/minor I
                              [idx idx])
            ;; if u is the zero vector, Q should just be the identity (BUG)
            u (dm/- x (dm/* alpha e))
            ;; the next two lines are exploding in code size
            ;; detected reuse since its u over norm u
            v (dm/d u (dmm/vnorm u))
            ;; detected reuse since v times v
            Q-step (dm/- I-step (dm/* 2 (dm/* v (dmm/t v))))
            ;; ideas for fixing
            ;; 1. do optimized substitutions
            ;; 2. matrix operated with a scalar is a place for reuse
            ;; 3. create new forms that do special things when emitted
            ;; 4. maybe a let form?
            ;; 5. precomputing values?
            ;; 6. at each qr-decomp-step evaluate expression?

            ;; this is to improve accuracy
            Q-step-1A (dmm/assoc-minor (dm/* Q-step
                                             A)
                                       [0 0]
                                       (dm/* e
                                             alpha))
            A' (dmm/minor Q-step-1A
                          [1 1])]
        (qr-decomp-step A'
                        I
                        I-size
                        (dm/* Q (dmm/assoc-minor I
                                                 [idx idx]
                                                 (dmm/t Q-step)))
                        (dmm/assoc-minor R
                                         [idx idx]
                                         Q-step-1A))))))

;; Will have to figure out how to fix the extra lets later
;; Also probably want a macro
;; (letm [internal binding, exp that will be auto-let'd] body)

(defn qr-decomp [A]
  (let [height (dmm/height A)
        I (dmm/identity-m height)]
    (qr-decomp-step A I height I I)))

(defn qr-decomp-step'
  [A I I-size Q R]
  (let [x (first (dmm/col-seq A))
        x-size (count x)
        idx (- I-size x-size)]
    (if (= 1 x-size)
      {:Q Q
       :R (dmm/assoc-minor R
                           [idx idx]
                           x)}
      (let [
            ;; computes reflection matrix given column vector x and e is a
            ;; column vector that is part of a basis of orthonormal vectors,
            ;; usually [1,0...0]^T
            e (dmm/t (dmm/v [(vec (concat [1]
                                          (repeat (dec x-size)
                                                  0)))]))
            ;; the sign of alpha should be opposite of kth coordinate of x, since
            ;; we want u to be as small as possible
            ;; you can use Math/copySign or in cuda copysignf
            alpha (dm/copy-sign (dmm/vnorm x)
                                (first (seq x)))
            I-step (dmm/minor I
                              [idx idx])
            ;; if u is the zero vector, Q should just be the identity (BUG)
            u (dm/- x (dm/* alpha e))
            ;; the next two lines are exploding in code size
            ;; detected reuse since its u over norm u
            ]
        (->> u
             (dm/letm (fn [u]
                        (let [v (dm/d u (dmm/vnorm u))
                              ;; detected reuse since v times v
                              ]
                          (->> v
                               (dm/letm (fn [v]
                                          (let [Q-step (dm/- I-step (dm/* 2 (dm/* v (dmm/t v))))
                                                ;; ideas for fixing
                                                ;; 1. do optimized substitutions
                                                ;; 2. matrix operated with a scalar is a place for reuse
                                                ;; 3. create new forms that do special things when emitted
                                                ;; 4. maybe a let form?
                                                ;; 5. precomputing values?
                                                ;; 6. at each qr-decomp-step evaluate expression?

                                                ;; this is to improve accuracy
                                                Q-step-1A (dmm/assoc-minor (dm/* Q-step
                                                                                 A)
                                                                           [0 0]
                                                                           (dm/* e
                                                                                 alpha))
                                                A' (dmm/minor Q-step-1A
                                                              [1 1])]
                                            (dm/letm (fn [A
                                                          Q
                                                          R]
                                                       (qr-decomp-step' A
                                                                        I
                                                                        I-size
                                                                        Q
                                                                        R))
                                                     A'
                                                     (dm/* Q (dmm/assoc-minor I
                                                                              [idx idx]
                                                                              (dmm/t Q-step)))
                                                     (dmm/assoc-minor R
                                                                      [idx idx]
                                                                      Q-step-1A))))))))))))))

(defn qr-decomp' [A]
  (let [height (dmm/height A)
        I (dmm/identity-m height)]
    (qr-decomp-step' A I height I I)))


(defn back-substitution
  "R must be in reduced row echelon form"
  [R b]
  (let [rows (vec (map (comp vec seq)
                       (dmm/row-seq R)))
        b (vec (seq b))]
    (rseq
     (reduce (fn [v i]
               (let [bi (b i)
                     ri (rows i)]
                 (conj v
                       (dm/d (dm/- bi (reduce dm/+
                                              0
                                              (map dm/*
                                                   (subvec ri
                                                           (inc i))
                                                   (rseq v))))
                             (ri i)))))
             []
             (reverse (range (count rows)))))))

(defn solve [A b]
  (let [{:keys [Q R]} (qr-decomp A)]
    (back-substitution R (dm/* (dmm/t Q)
                               b))))
(let [[a b c d e f g h i] (map (fn [n]
                                 (dmp/s {:variable (str "m" n)}))
                               (range 9))]
  (-> (qr-decomp' (dmm/v [[a b c]
                          [d e f]
                          [g h i]]))
      dmp/emit
      user/re))