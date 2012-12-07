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
      (let [;; computes reflection matrix given column vector x and e is a
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
                              [idx idx])]
        (dm/letm [;; if u is the zero vector, Q should just be the identity (BUG)
                  u (dm/- x (dm/* alpha e))]
          (dm/letm [v (dm/d u (dmm/vnorm u))]
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
              (dm/letm [A A'
                        Q (dm/* Q (dmm/assoc-minor I
                                                 [idx idx]
                                                 (dmm/t Q-step)))
                        R (dmm/assoc-minor R
                                           [idx idx]
                                           Q-step-1A)]
                       (qr-decomp-step' A
                                        I
                                        I-size
                                        Q
                                        R)))))))))

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
    (dm/letm [result (reduce (fn [v' i]
                                (let [bi (b i)
                                      ri (rows i)]
                                  (conj v'
                                        (dm/letm [v v']
                                                 (dm/d (dm/- bi
                                                             (reduce dm/+
                                                                     0
                                                                     (map dm/*
                                                                          (subvec ri
                                                                                  (inc i))
                                                                          (rseq v))))
                                                       (ri i))))))
                              []
                              (reverse (range (count rows))))]
             (dmm/v (vec (rseq result))))))

(defn solve [A b]
  (let [{:keys [Q R]} (qr-decomp A)]
    (back-substitution R (dm/* (dmm/t Q)
                               b))))

(defn solve' [A b]
  (dm/letm [results (qr-decomp' A)]
           (let [{:keys [Q R]} results]
             (back-substitution R (dm/* (dmm/t Q)
                                        b)))))

(let [[a b c d e f g h i] (map (fn [n]
                                    (dmp/s {:variable (str "A" n)}))
                                  (range 9))
         [x y z] (map (fn [n]
                        (dmp/s {:variable (str "b" n)}))
                      (range 3))]
     (-> #_ (qr-decomp' (dmm/v [[a b c]
                             [d e f]
                             [g h i]]))
         #_ (qr-decomp' (dmm/v [[a b]
                                [c d]]))
         (solve' (dmm/v [[a b c]
                         [d e f]
                         [g h i]])
                 (dmm/t (dmm/v [[x y z]])))
         dj.math.cemit/emit
         user/re))

;; solution [5 3 -2]
#_ (let [[A0 A1 A2 A3 A4 A5 A6 A7 A8 A9] [1.0 1.0 1.0 0.0 2.0 5.0 2.0 5.0 -1.0]
         [b0 b1 b2] [6.0 -4.0 27.0]]
     (let [m0_03DA8 (- A0 (Math/copySign (Math/sqrt (+ (+ (Math/pow A0 2) (Math/pow A3 2)) (Math/pow A6 2))) A0)) m1_03DA9 A3 m2_03DAA A6 m0_03DAB (/ m0_03DA8 (Math/sqrt (+ (+ (Math/pow m0_03DA8 2) (Math/pow m1_03DA9 2)) (Math/pow m2_03DAA 2)))) m1_03DAC (/ m1_03DA9 (Math/sqrt (+ (+ (Math/pow m0_03DA8 2) (Math/pow m1_03DA9 2)) (Math/pow m2_03DAA 2)))) m2_03DAD (/ m2_03DAA (Math/sqrt (+ (+ (Math/pow m0_03DA8 2) (Math/pow m1_03DA9 2)) (Math/pow m2_03DAA 2)))) m0_03DAE (+ (+ (* (- (* (* m1_03DAC m0_03DAB) 2)) A1) (* (- 1 (* (* m1_03DAC m1_03DAC) 2)) A4)) (* (- (* (* m1_03DAC m2_03DAD) 2)) A7)) m1_03DAF (+ (+ (* (- (* (* m1_03DAC m0_03DAB) 2)) A2) (* (- 1 (* (* m1_03DAC m1_03DAC) 2)) A5)) (* (- (* (* m1_03DAC m2_03DAD) 2)) A8)) m2_03DB0 (+ (+ (* (- (* (* m2_03DAD m0_03DAB) 2)) A1) (* (- (* (* m2_03DAD m1_03DAC) 2)) A4)) (* (- 1 (* (* m2_03DAD m2_03DAD) 2)) A7)) m3_03DB1 (+ (+ (* (- (* (* m2_03DAD m0_03DAB) 2)) A2) (* (- (* (* m2_03DAD m1_03DAC) 2)) A5)) (* (- 1 (* (* m2_03DAD m2_03DAD) 2)) A8)) m0_03DB2 (- 1 (* (* m0_03DAB m0_03DAB) 2)) m1_03DB3 (- (* (* m1_03DAC m0_03DAB) 2)) m2_03DB4 (- (* (* m2_03DAD m0_03DAB) 2)) m3_03DB5 (- (* (* m0_03DAB m1_03DAC) 2)) m4_03DB6 (- 1 (* (* m1_03DAC m1_03DAC) 2)) m5_03DB7 (- (* (* m2_03DAD m1_03DAC) 2)) m6_03DB8 (- (* (* m0_03DAB m2_03DAD) 2)) m7_03DB9 (- (* (* m1_03DAC m2_03DAD) 2)) m8_03DBA (- 1 (* (* m2_03DAD m2_03DAD) 2)) m0_03DBB (Math/copySign (Math/sqrt (+ (+ (Math/pow A0 2) (Math/pow A3 2)) (Math/pow A6 2))) A0) m1_03DBC (+ (+ (* (- 1 (* (* m0_03DAB m0_03DAB) 2)) A1) (* (- (* (* m0_03DAB m1_03DAC) 2)) A4)) (* (- (* (* m0_03DAB m2_03DAD) 2)) A7)) m2_03DBD (+ (+ (* (- 1 (* (* m0_03DAB m0_03DAB) 2)) A2) (* (- (* (* m0_03DAB m1_03DAC) 2)) A5)) (* (- (* (* m0_03DAB m2_03DAD) 2)) A8)) m4_03DBF (+ (+ (* (- (* (* m1_03DAC m0_03DAB) 2)) A1) (* (- 1 (* (* m1_03DAC m1_03DAC) 2)) A4)) (* (- (* (* m1_03DAC m2_03DAD) 2)) A7)) m5_03DC0 (+ (+ (* (- (* (* m1_03DAC m0_03DAB) 2)) A2) (* (- 1 (* (* m1_03DAC m1_03DAC) 2)) A5)) (* (- (* (* m1_03DAC m2_03DAD) 2)) A8)) m7_03DC2 (+ (+ (* (- (* (* m2_03DAD m0_03DAB) 2)) A1) (* (- (* (* m2_03DAD m1_03DAC) 2)) A4)) (* (- 1 (* (* m2_03DAD m2_03DAD) 2)) A7)) m8_03DC3 (+ (+ (* (- (* (* m2_03DAD m0_03DAB) 2)) A2) (* (- (* (* m2_03DAD m1_03DAC) 2)) A5)) (* (- 1 (* (* m2_03DAD m2_03DAD) 2)) A8)) m0_03DC4 (- m0_03DAE (Math/copySign (Math/sqrt (+ (Math/pow m0_03DAE 2) (Math/pow m2_03DB0 2))) m0_03DAE)) m1_03DC5 m2_03DB0 m0_03DC6 (/ m0_03DC4 (Math/sqrt (+ (Math/pow m0_03DC4 2) (Math/pow m1_03DC5 2)))) m1_03DC7 (/ m1_03DC5 (Math/sqrt (+ (Math/pow m0_03DC4 2) (Math/pow m1_03DC5 2)))) m0_03DC8 (+ (* (- (* (* m1_03DC7 m0_03DC6) 2)) m1_03DAF) (* (- 1 (* (* m1_03DC7 m1_03DC7) 2)) m3_03DB1)) m0_03DC9 m0_03DB2 m1_03DCA (+ (* m1_03DB3 (- 1 (* (* m0_03DC6 m0_03DC6) 2))) (* m2_03DB4 (- (* (* m0_03DC6 m1_03DC7) 2)))) m2_03DCB (+ (* m1_03DB3 (- (* (* m1_03DC7 m0_03DC6) 2))) (* m2_03DB4 (- 1 (* (* m1_03DC7 m1_03DC7) 2)))) m3_03DCC m3_03DB5 m4_03DCD (+ (* m4_03DB6 (- 1 (* (* m0_03DC6 m0_03DC6) 2))) (* m5_03DB7 (- (* (* m0_03DC6 m1_03DC7) 2)))) m5_03DCE (+ (* m4_03DB6 (- (* (* m1_03DC7 m0_03DC6) 2))) (* m5_03DB7 (- 1 (* (* m1_03DC7 m1_03DC7) 2)))) m6_03DCF m6_03DB8 m7_03DD0 (+ (* m7_03DB9 (- 1 (* (* m0_03DC6 m0_03DC6) 2))) (* m8_03DBA (- (* (* m0_03DC6 m1_03DC7) 2)))) m8_03DD1 (+ (* m7_03DB9 (- (* (* m1_03DC7 m0_03DC6) 2))) (* m8_03DBA (- 1 (* (* m1_03DC7 m1_03DC7) 2)))) m0_03DD2 m0_03DBB m1_03DD3 m1_03DBC m2_03DD4 m2_03DBD m4_03DD6 (Math/copySign (Math/sqrt (+ (Math/pow m0_03DAE 2) (Math/pow m2_03DB0 2))) m0_03DAE) m5_03DD7 (+ (* (- 1 (* (* m0_03DC6 m0_03DC6) 2)) m1_03DAF) (* (- (* (* m0_03DC6 m1_03DC7) 2)) m3_03DB1)) m8_03DDA (+ (* (- (* (* m1_03DC7 m0_03DC6) 2)) m1_03DAF) (* (- 1 (* (* m1_03DC7 m1_03DC7) 2)) m3_03DB1))] [[(/ (- (+ (+ (* m0_03DC9 b0) (* m3_03DCC b1)) (* m6_03DCF b2)) (+ (* m1_03DD3 (/ (- (+ (+ (* m1_03DCA b0) (* m4_03DCD b1)) (* m7_03DD0 b2)) (* m5_03DD7 (/ (+ (+ (* m2_03DCB b0) (* m5_03DCE b1)) (* m8_03DD1 b2)) m0_03DC8))) m4_03DD6)) (* m2_03DD4 (/ (+ (+ (* m2_03DCB b0) (* m5_03DCE b1)) (* m8_03DD1 b2)) m0_03DC8)))) m0_03DD2) (/ (- (+ (+ (* m1_03DCA b0) (* m4_03DCD b1)) (* m7_03DD0 b2)) (* m5_03DD7 (/ (+ (+ (* m2_03DCB b0) (* m5_03DCE b1)) (* m8_03DD1 b2)) m0_03DC8))) m4_03DD6) (/ (+ (+ (* m2_03DCB b0) (* m5_03DCE b1)) (* m8_03DD1 b2)) m0_03DC8)]]))

