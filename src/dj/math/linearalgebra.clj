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
        (dm/letm [
                  ;; if u is the zero vector, Q should just be the identity (BUG)

                  ;; for now let's try just compile time detection
                  u (dm/- x (dm/* alpha e))]
                 (if (let [n (dmm/vnorm u)]
                       (and (number? n)
                            (zero? n)))
                   (let [Q-step I-step
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
                                               R)))
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
                                                        R))))))))))

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
                     (dmp/s {:op "var"
                             :name (dm/gensym (str "v" n))}))
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
                                   (dmp/s {:op "var"
                                           :name (str "A" n)}))
                                 (range 9))
        [x y z] (map (fn [n]
                       (dmp/s {:op "var"
                               :name (str "b" n)}))
                     (range 3))
        emitter (dj.math.cemit/c-emitter)]
    (-> #_ (qr-decomp' (dmm/v [[a b c]
                            [d e f]
                            [g h i]]))
        #_ (qr-decomp' (dmm/v [[a b]
                               [c d]]))
        (solve' (dmm/v [[a b c]
                        [d e f]
                        [g h i]])
                (dmm/t (dmm/v [[x y z]])))
        #_ dj.math.cemit/emit
        emitter
        user/re)))

;; solution [5 3 -2]
#_ (let [[A0 A1 A2 A3 A4 A5 A6 A7 A8 A9] [1.0 1.0 1.0 0.0 2.0 5.0 2.0 5.0 -1.0]
         [b0 b1 b2] [6.0 -4.0 27.0]]
     (let [m0_0014B (- A0 (Math/copySign (Math/sqrt (+ (+ (clojure.core/* A0 A0) (clojure.core/* A3 A3)) (clojure.core/* A6 A6))) A0)) m1_0014C A3 m2_0014D A6 m0_0014E (/ m0_0014B (Math/sqrt (+ (+ (clojure.core/* m0_0014B m0_0014B) (clojure.core/* m1_0014C m1_0014C)) (clojure.core/* m2_0014D m2_0014D)))) m1_0014F (/ m1_0014C (Math/sqrt (+ (+ (clojure.core/* m0_0014B m0_0014B) (clojure.core/* m1_0014C m1_0014C)) (clojure.core/* m2_0014D m2_0014D)))) m2_00150 (/ m2_0014D (Math/sqrt (+ (+ (clojure.core/* m0_0014B m0_0014B) (clojure.core/* m1_0014C m1_0014C)) (clojure.core/* m2_0014D m2_0014D)))) m0_00151 (+ (+ (* (- (* (* m1_0014F m0_0014E) 2)) A1) (* (- 1 (* (* m1_0014F m1_0014F) 2)) A4)) (* (- (* (* m1_0014F m2_00150) 2)) A7)) m1_00152 (+ (+ (* (- (* (* m1_0014F m0_0014E) 2)) A2) (* (- 1 (* (* m1_0014F m1_0014F) 2)) A5)) (* (- (* (* m1_0014F m2_00150) 2)) A8)) m2_00153 (+ (+ (* (- (* (* m2_00150 m0_0014E) 2)) A1) (* (- (* (* m2_00150 m1_0014F) 2)) A4)) (* (- 1 (* (* m2_00150 m2_00150) 2)) A7)) m3_00154 (+ (+ (* (- (* (* m2_00150 m0_0014E) 2)) A2) (* (- (* (* m2_00150 m1_0014F) 2)) A5)) (* (- 1 (* (* m2_00150 m2_00150) 2)) A8)) m0_00155 (- 1 (* (* m0_0014E m0_0014E) 2)) m1_00156 (- (* (* m1_0014F m0_0014E) 2)) m2_00157 (- (* (* m2_00150 m0_0014E) 2)) m3_00158 (- (* (* m0_0014E m1_0014F) 2)) m4_00159 (- 1 (* (* m1_0014F m1_0014F) 2)) m5_0015A (- (* (* m2_00150 m1_0014F) 2)) m6_0015B (- (* (* m0_0014E m2_00150) 2)) m7_0015C (- (* (* m1_0014F m2_00150) 2)) m8_0015D (- 1 (* (* m2_00150 m2_00150) 2)) m0_0015E (Math/copySign (Math/sqrt (+ (+ (clojure.core/* A0 A0) (clojure.core/* A3 A3)) (clojure.core/* A6 A6))) A0) m1_0015F (+ (+ (* (- 1 (* (* m0_0014E m0_0014E) 2)) A1) (* (- (* (* m0_0014E m1_0014F) 2)) A4)) (* (- (* (* m0_0014E m2_00150) 2)) A7)) m2_00160 (+ (+ (* (- 1 (* (* m0_0014E m0_0014E) 2)) A2) (* (- (* (* m0_0014E m1_0014F) 2)) A5)) (* (- (* (* m0_0014E m2_00150) 2)) A8)) m4_00162 (+ (+ (* (- (* (* m1_0014F m0_0014E) 2)) A1) (* (- 1 (* (* m1_0014F m1_0014F) 2)) A4)) (* (- (* (* m1_0014F m2_00150) 2)) A7)) m5_00163 (+ (+ (* (- (* (* m1_0014F m0_0014E) 2)) A2) (* (- 1 (* (* m1_0014F m1_0014F) 2)) A5)) (* (- (* (* m1_0014F m2_00150) 2)) A8)) m7_00165 (+ (+ (* (- (* (* m2_00150 m0_0014E) 2)) A1) (* (- (* (* m2_00150 m1_0014F) 2)) A4)) (* (- 1 (* (* m2_00150 m2_00150) 2)) A7)) m8_00166 (+ (+ (* (- (* (* m2_00150 m0_0014E) 2)) A2) (* (- (* (* m2_00150 m1_0014F) 2)) A5)) (* (- 1 (* (* m2_00150 m2_00150) 2)) A8)) m0_00167 (- m0_00151 (Math/copySign (Math/sqrt (+ (clojure.core/* m0_00151 m0_00151) (clojure.core/* m2_00153 m2_00153))) m0_00151)) m1_00168 m2_00153 m0_00169 (/ m0_00167 (Math/sqrt (+ (clojure.core/* m0_00167 m0_00167) (clojure.core/* m1_00168 m1_00168)))) m1_0016A (/ m1_00168 (Math/sqrt (+ (clojure.core/* m0_00167 m0_00167) (clojure.core/* m1_00168 m1_00168)))) m0_0016B (+ (* (- (* (* m1_0016A m0_00169) 2)) m1_00152) (* (- 1 (* (* m1_0016A m1_0016A) 2)) m3_00154)) m0_0016C m0_00155 m1_0016D (+ (* m1_00156 (- 1 (* (* m0_00169 m0_00169) 2))) (* m2_00157 (- (* (* m0_00169 m1_0016A) 2)))) m2_0016E (+ (* m1_00156 (- (* (* m1_0016A m0_00169) 2))) (* m2_00157 (- 1 (* (* m1_0016A m1_0016A) 2)))) m3_0016F m3_00158 m4_00170 (+ (* m4_00159 (- 1 (* (* m0_00169 m0_00169) 2))) (* m5_0015A (- (* (* m0_00169 m1_0016A) 2)))) m5_00171 (+ (* m4_00159 (- (* (* m1_0016A m0_00169) 2))) (* m5_0015A (- 1 (* (* m1_0016A m1_0016A) 2)))) m6_00172 m6_0015B m7_00173 (+ (* m7_0015C (- 1 (* (* m0_00169 m0_00169) 2))) (* m8_0015D (- (* (* m0_00169 m1_0016A) 2)))) m8_00174 (+ (* m7_0015C (- (* (* m1_0016A m0_00169) 2))) (* m8_0015D (- 1 (* (* m1_0016A m1_0016A) 2)))) m0_00175 m0_0015E m1_00176 m1_0015F m2_00177 m2_00160 m4_00179 (Math/copySign (Math/sqrt (+ (clojure.core/* m0_00151 m0_00151) (clojure.core/* m2_00153 m2_00153))) m0_00151) m5_0017A (+ (* (- 1 (* (* m0_00169 m0_00169) 2)) m1_00152) (* (- (* (* m0_00169 m1_0016A) 2)) m3_00154)) m8_0017D (+ (* (- (* (* m1_0016A m0_00169) 2)) m1_00152) (* (- 1 (* (* m1_0016A m1_0016A) 2)) m3_00154)) v2_00181 (/ (+ (+ (* m2_0016E b0) (* m5_00171 b1)) (* m8_00174 b2)) m0_0016B) v1_00180 (/ (- (+ (+ (* m1_0016D b0) (* m4_00170 b1)) (* m7_00173 b2)) (* m5_0017A v2_00181)) m4_00179) v0_0017F (/ (- (+ (+ (* m0_0016C b0) (* m3_0016F b1)) (* m6_00172 b2)) (+ (* m2_00177 v2_00181) (* m1_00176 v1_00180))) m0_00175)] [v0_0017F v1_00180 v2_00181]))
