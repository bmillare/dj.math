(ns dj.math.linearalgebra
  (:require [dj.math :as dm]
            [dj.math.matrix :as dmm]
            [dj.math.parser :as dmp]
            [dj.math.bindings :as dmb]))

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
    (dmm/v [(vec (rseq (reduce (fn [v i]
                                 (let [bi (b i)
                                       ri (rows i)]
                                   (conj v
                                         (dm/d (dm/- bi
                                                     (reduce dm/+
                                                             0
                                                             (map dm/*
                                                                  (reverse
                                                                   (subvec ri
                                                                           (inc i)))
                                                                  v)))
                                               (ri i)))))
                               []
                               (reverse (range (count rows))))))])))

(defn back-substitution'
  "R must be in reduced row echelon form"
  [R b]
  (let [rows (vec (map (comp vec seq)
                       (dmm/row-seq R)))
        b (vec (seq b))
        syms (mapv (fn [n]
                     (dmp/s {:variable (dm/gensym (str "v" n))}))
                   (range (count rows)))
        es (reduce (fn [bs i]
                     (let [bi (b i)
                           ri (rows i)]
                       (conj bs
                             (dm/d (dm/- bi
                                         (reduce dm/+
                                                 0
                                                 (map dm/*
                                                      (reverse
                                                       (subvec ri
                                                               (inc i)))
                                                      (rseq syms))))
                                   (ri i)))))
                   []
                   (reverse (range (count rows))))]
    ;; Weird bug here, can't have vec inside children???
    ;; probably has to do with letm
    (dmp/s {:op "let"
            :bindings (dmb/zip->bindings (rseq syms)
                                         es)
            :children [(vec syms)]})))

(defn solve [A b]
  (let [{:keys [Q R]} (qr-decomp A)]
    (back-substitution R (dm/* (dmm/t Q)
                               b))))

(defn solve' [A b]
  (dm/letm [results (qr-decomp' A)]
           (let [{:keys [Q R]} results]
             (back-substitution' R (dm/* (dmm/t Q)
                                         b)))))

(defn test-run []
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
        #_ (solve' (dmm/v [[a b c]
                        [d e f]
                        [g h i]])
                (dmm/t (dmm/v [[x y z]])))
        #_ dj.math.cemit/emit
        dj.math.parser/emit
        user/re)))

;; solution [5 3 -2]
#_ (let [[A0 A1 A2 A3 A4 A5 A6 A7 A8 A9] [1.0 1.0 1.0 0.0 2.0 5.0 2.0 5.0 -1.0]
         [b0 b1 b2] [6.0 -4.0 27.0]]
     (let [m0_00034 (- A0 (Math/copySign (Math/sqrt (+ (+ (Math/pow A0 2) (Math/pow A3 2)) (Math/pow A6 2))) A0))
           m0_00037 (/ m0_00034 (Math/sqrt (+ (+ (Math/pow m0_00034 2) (Math/pow A3 2)) (Math/pow A6 2))))
           m1_00038 (/ A3 (Math/sqrt (+ (+ (Math/pow m0_00034 2) (Math/pow A3 2)) (Math/pow A6 2))))
           m2_00039 (/ A6 (Math/sqrt (+ (+ (Math/pow m0_00034 2) (Math/pow A3 2)) (Math/pow A6 2))))
           m0_0003A (+ (+ (* (- (* (* m1_00038 m0_00037) 2)) A1) (* (- 1 (* (* m1_00038 m1_00038) 2)) A4)) (* (- (* (* m1_00038 m2_00039) 2)) A7))
           m1_0003B (+ (+ (* (- (* (* m1_00038 m0_00037) 2)) A2) (* (- 1 (* (* m1_00038 m1_00038) 2)) A5)) (* (- (* (* m1_00038 m2_00039) 2)) A8))
           m2_0003C (+ (+ (* (- (* (* m2_00039 m0_00037) 2)) A1) (* (- (* (* m2_00039 m1_00038) 2)) A4)) (* (- 1 (* (* m2_00039 m2_00039) 2)) A7))
           m3_0003D (+ (+ (* (- (* (* m2_00039 m0_00037) 2)) A2) (* (- (* (* m2_00039 m1_00038) 2)) A5)) (* (- 1 (* (* m2_00039 m2_00039) 2)) A8))
           m0_0003E (- 1 (* (* m0_00037 m0_00037) 2))
           m1_0003F (- (* (* m1_00038 m0_00037) 2))
           m2_00040 (- (* (* m2_00039 m0_00037) 2))
           m3_00041 (- (* (* m0_00037 m1_00038) 2))
           m4_00042 (- 1 (* (* m1_00038 m1_00038) 2))
           m5_00043 (- (* (* m2_00039 m1_00038) 2))
           m6_00044 (- (* (* m0_00037 m2_00039) 2))
           m7_00045 (- (* (* m1_00038 m2_00039) 2))
           m8_00046 (- 1 (* (* m2_00039 m2_00039) 2))
           m0_00047 (Math/copySign (Math/sqrt (+ (+ (Math/pow A0 2) (Math/pow A3 2)) (Math/pow A6 2))) A0)
           m1_00048 (+ (+ (* (- 1 (* (* m0_00037 m0_00037) 2)) A1) (* (- (* (* m0_00037 m1_00038) 2)) A4)) (* (- (* (* m0_00037 m2_00039) 2)) A7))
           m2_00049 (+ (+ (* (- 1 (* (* m0_00037 m0_00037) 2)) A2) (* (- (* (* m0_00037 m1_00038) 2)) A5)) (* (- (* (* m0_00037 m2_00039) 2)) A8))
           m4_0004B (+ (+ (* (- (* (* m1_00038 m0_00037) 2)) A1) (* (- 1 (* (* m1_00038 m1_00038) 2)) A4)) (* (- (* (* m1_00038 m2_00039) 2)) A7))
           m5_0004C (+ (+ (* (- (* (* m1_00038 m0_00037) 2)) A2) (* (- 1 (* (* m1_00038 m1_00038) 2)) A5)) (* (- (* (* m1_00038 m2_00039) 2)) A8))
           m7_0004E (+ (+ (* (- (* (* m2_00039 m0_00037) 2)) A1) (* (- (* (* m2_00039 m1_00038) 2)) A4)) (* (- 1 (* (* m2_00039 m2_00039) 2)) A7))
           m8_0004F (+ (+ (* (- (* (* m2_00039 m0_00037) 2)) A2) (* (- (* (* m2_00039 m1_00038) 2)) A5)) (* (- 1 (* (* m2_00039 m2_00039) 2)) A8))
           m0_00050 (- m0_0003A (Math/copySign (Math/sqrt (+ (Math/pow m0_0003A 2) (Math/pow m2_0003C 2))) m0_0003A))
           m0_00052 (/ m0_00050 (Math/sqrt (+ (Math/pow m0_00050 2) (Math/pow m2_0003C 2))))
           m1_00053 (/ m2_0003C (Math/sqrt (+ (Math/pow m0_00050 2) (Math/pow m2_0003C 2))))
           m0_00054 (+ (* (- (* (* m1_00053 m0_00052) 2)) m1_0003B) (* (- 1 (* (* m1_00053 m1_00053) 2)) m3_0003D))
           m1_00056 (+ (* m1_0003F (- 1 (* (* m0_00052 m0_00052) 2))) (* m2_00040 (- (* (* m0_00052 m1_00053) 2))))
           m2_00057 (+ (* m1_0003F (- (* (* m1_00053 m0_00052) 2))) (* m2_00040 (- 1 (* (* m1_00053 m1_00053) 2))))
           m4_00059 (+ (* m4_00042 (- 1 (* (* m0_00052 m0_00052) 2))) (* m5_00043 (- (* (* m0_00052 m1_00053) 2))))
           m5_0005A (+ (* m4_00042 (- (* (* m1_00053 m0_00052) 2))) (* m5_00043 (- 1 (* (* m1_00053 m1_00053) 2))))
           m7_0005C (+ (* m7_00045 (- 1 (* (* m0_00052 m0_00052) 2))) (* m8_00046 (- (* (* m0_00052 m1_00053) 2))))
           m8_0005D (+ (* m7_00045 (- (* (* m1_00053 m0_00052) 2))) (* m8_00046 (- 1 (* (* m1_00053 m1_00053) 2))))
           m4_00062 (Math/copySign (Math/sqrt (+ (Math/pow m0_0003A 2) (Math/pow m2_0003C 2))) m0_0003A)
           m5_00063 (+ (* (- 1 (* (* m0_00052 m0_00052) 2)) m1_0003B) (* (- (* (* m0_00052 m1_00053) 2)) m3_0003D))
           m8_00066 (+ (* (- (* (* m1_00053 m0_00052) 2)) m1_0003B) (* (- 1 (* (* m1_00053 m1_00053) 2)) m3_0003D))
           v2_0006A (/ (+ (+ (* m2_00057 b0) (* m5_0005A b1)) (* m8_0005D b2)) m0_00054)
           v1_00069 (/ (- (+ (+ (* m1_00056 b0) (* m4_00059 b1)) (* m7_0005C b2)) (* m5_00063 v2_0006A)) m4_00062)
           v0_00068 (/ (- (+ (+ (* m0_0003E b0) (* m3_00041 b1)) (* m6_00044 b2)) (+ (* m1_00048 v2_0006A) (* m2_00049 v1_00069))) m0_00047)]
       [(/ (- (+ (+ (* m0_0003E b0) (* m3_00041 b1)) (* m6_00044 b2)) (+ (* m1_00048 v2_0006A) (* m2_00049 v1_00069))) m0_00047)
        (/ (- (+ (+ (* m1_00056 b0) (* m4_00059 b1)) (* m7_0005C b2)) (* m5_00063 v2_0006A)) m4_00062)
        (/ (+ (+ (* m2_00057 b0) (* m5_0005A b1)) (* m8_0005D b2)) m0_00054)]))

#_ [17 3 -2]