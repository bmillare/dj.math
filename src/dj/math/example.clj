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
    (dmm/v (vec (rseq (reduce (fn [v i]
                                (let [bi (b i)
                                      ri (rows i)]
                                  (conj v
                                        (dm/d (dm/- bi
                                                    (reduce dm/+
                                                            0
                                                            (map dm/*
                                                                 (subvec ri
                                                                         (inc i))
                                                                 (rseq v))))
                                              (ri i)))))
                              []
                              (reverse (range (count rows)))))))))

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
                                                      (subvec ri
                                                              (inc i))
                                                      (rseq syms))))
                                   (ri i)))))
                   []
                   (reverse (range (count rows))))]
    ;; Weird bug here, can't have vec inside children???
    #_ (dmp/s {:op "let"
            :bindings (vec (mapcat vector
                                   (rseq syms)
                                   es))
               :children [(vec (rseq es))]})
    (dmp/s {:op "let"
            :bindings (vec (mapcat vector
                                   (rseq syms)
                                   es))
            :children syms})))

(defn solve [A b]
  (let [{:keys [Q R]} (qr-decomp A)]
    (back-substitution R (dm/* (dmm/t Q)
                               b))))

(defn solve' [A b]
  (dm/letm [results (qr-decomp' A)]
           (let [{:keys [Q R]} results]
             (back-substitution' R (dm/* (dmm/t Q)
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
         #_ dj.math.parser/emit
         user/re))

;; solution [5 3 -2]
#_ (let [[A0 A1 A2 A3 A4 A5 A6 A7 A8 A9] [1.0 1.0 1.0 0.0 2.0 5.0 2.0 5.0 -1.0]
         [b0 b1 b2] [6.0 -4.0 27.0]]
     (let [m0_07F54 (- A0 (Math/copySign (Math/sqrt (+ (+ (Math/pow A0 2) (Math/pow A3 2)) (Math/pow A6 2))) A0)) m1_07F55 A3 m2_07F56 A6 m0_07F57 (/ m0_07F54 (Math/sqrt (+ (+ (Math/pow m0_07F54 2) (Math/pow m1_07F55 2)) (Math/pow m2_07F56 2)))) m1_07F58 (/ m1_07F55 (Math/sqrt (+ (+ (Math/pow m0_07F54 2) (Math/pow m1_07F55 2)) (Math/pow m2_07F56 2)))) m2_07F59 (/ m2_07F56 (Math/sqrt (+ (+ (Math/pow m0_07F54 2) (Math/pow m1_07F55 2)) (Math/pow m2_07F56 2)))) m0_07F5A (+ (+ (* (- (* (* m1_07F58 m0_07F57) 2)) A1) (* (- 1 (* (* m1_07F58 m1_07F58) 2)) A4)) (* (- (* (* m1_07F58 m2_07F59) 2)) A7)) m1_07F5B (+ (+ (* (- (* (* m1_07F58 m0_07F57) 2)) A2) (* (- 1 (* (* m1_07F58 m1_07F58) 2)) A5)) (* (- (* (* m1_07F58 m2_07F59) 2)) A8)) m2_07F5C (+ (+ (* (- (* (* m2_07F59 m0_07F57) 2)) A1) (* (- (* (* m2_07F59 m1_07F58) 2)) A4)) (* (- 1 (* (* m2_07F59 m2_07F59) 2)) A7)) m3_07F5D (+ (+ (* (- (* (* m2_07F59 m0_07F57) 2)) A2) (* (- (* (* m2_07F59 m1_07F58) 2)) A5)) (* (- 1 (* (* m2_07F59 m2_07F59) 2)) A8)) m0_07F5E (- 1 (* (* m0_07F57 m0_07F57) 2)) m1_07F5F (- (* (* m1_07F58 m0_07F57) 2)) m2_07F60 (- (* (* m2_07F59 m0_07F57) 2)) m3_07F61 (- (* (* m0_07F57 m1_07F58) 2)) m4_07F62 (- 1 (* (* m1_07F58 m1_07F58) 2)) m5_07F63 (- (* (* m2_07F59 m1_07F58) 2)) m6_07F64 (- (* (* m0_07F57 m2_07F59) 2)) m7_07F65 (- (* (* m1_07F58 m2_07F59) 2)) m8_07F66 (- 1 (* (* m2_07F59 m2_07F59) 2)) m0_07F67 (Math/copySign (Math/sqrt (+ (+ (Math/pow A0 2) (Math/pow A3 2)) (Math/pow A6 2))) A0) m1_07F68 (+ (+ (* (- 1 (* (* m0_07F57 m0_07F57) 2)) A1) (* (- (* (* m0_07F57 m1_07F58) 2)) A4)) (* (- (* (* m0_07F57 m2_07F59) 2)) A7)) m2_07F69 (+ (+ (* (- 1 (* (* m0_07F57 m0_07F57) 2)) A2) (* (- (* (* m0_07F57 m1_07F58) 2)) A5)) (* (- (* (* m0_07F57 m2_07F59) 2)) A8)) m4_07F6B (+ (+ (* (- (* (* m1_07F58 m0_07F57) 2)) A1) (* (- 1 (* (* m1_07F58 m1_07F58) 2)) A4)) (* (- (* (* m1_07F58 m2_07F59) 2)) A7)) m5_07F6C (+ (+ (* (- (* (* m1_07F58 m0_07F57) 2)) A2) (* (- 1 (* (* m1_07F58 m1_07F58) 2)) A5)) (* (- (* (* m1_07F58 m2_07F59) 2)) A8)) m7_07F6E (+ (+ (* (- (* (* m2_07F59 m0_07F57) 2)) A1) (* (- (* (* m2_07F59 m1_07F58) 2)) A4)) (* (- 1 (* (* m2_07F59 m2_07F59) 2)) A7)) m8_07F6F (+ (+ (* (- (* (* m2_07F59 m0_07F57) 2)) A2) (* (- (* (* m2_07F59 m1_07F58) 2)) A5)) (* (- 1 (* (* m2_07F59 m2_07F59) 2)) A8)) m0_07F70 (- m0_07F5A (Math/copySign (Math/sqrt (+ (Math/pow m0_07F5A 2) (Math/pow m2_07F5C 2))) m0_07F5A)) m1_07F71 m2_07F5C m0_07F72 (/ m0_07F70 (Math/sqrt (+ (Math/pow m0_07F70 2) (Math/pow m1_07F71 2)))) m1_07F73 (/ m1_07F71 (Math/sqrt (+ (Math/pow m0_07F70 2) (Math/pow m1_07F71 2)))) m0_07F74 (+ (* (- (* (* m1_07F73 m0_07F72) 2)) m1_07F5B) (* (- 1 (* (* m1_07F73 m1_07F73) 2)) m3_07F5D)) m0_07F75 m0_07F5E m1_07F76 (+ (* m1_07F5F (- 1 (* (* m0_07F72 m0_07F72) 2))) (* m2_07F60 (- (* (* m0_07F72 m1_07F73) 2)))) m2_07F77 (+ (* m1_07F5F (- (* (* m1_07F73 m0_07F72) 2))) (* m2_07F60 (- 1 (* (* m1_07F73 m1_07F73) 2)))) m3_07F78 m3_07F61 m4_07F79 (+ (* m4_07F62 (- 1 (* (* m0_07F72 m0_07F72) 2))) (* m5_07F63 (- (* (* m0_07F72 m1_07F73) 2)))) m5_07F7A (+ (* m4_07F62 (- (* (* m1_07F73 m0_07F72) 2))) (* m5_07F63 (- 1 (* (* m1_07F73 m1_07F73) 2)))) m6_07F7B m6_07F64 m7_07F7C (+ (* m7_07F65 (- 1 (* (* m0_07F72 m0_07F72) 2))) (* m8_07F66 (- (* (* m0_07F72 m1_07F73) 2)))) m8_07F7D (+ (* m7_07F65 (- (* (* m1_07F73 m0_07F72) 2))) (* m8_07F66 (- 1 (* (* m1_07F73 m1_07F73) 2)))) m0_07F7E m0_07F67 m1_07F7F m1_07F68 m2_07F80 m2_07F69 m4_07F82 (Math/copySign (Math/sqrt (+ (Math/pow m0_07F5A 2) (Math/pow m2_07F5C 2))) m0_07F5A) m5_07F83 (+ (* (- 1 (* (* m0_07F72 m0_07F72) 2)) m1_07F5B) (* (- (* (* m0_07F72 m1_07F73) 2)) m3_07F5D)) m8_07F86 (+ (* (- (* (* m1_07F73 m0_07F72) 2)) m1_07F5B) (* (- 1 (* (* m1_07F73 m1_07F73) 2)) m3_07F5D)) v2_07F8A (/ (+ (+ (* m2_07F77 b0) (* m5_07F7A b1)) (* m8_07F7D b2)) m0_07F74) v1_07F89 (/ (- (+ (+ (* m1_07F76 b0) (* m4_07F79 b1)) (* m7_07F7C b2)) (* m5_07F83 v2_07F8A)) m4_07F82) v0_07F88 (/ (- (+ (+ (* m0_07F75 b0) (* m3_07F78 b1)) (* m6_07F7B b2)) (+ (* m1_07F7F v2_07F8A) (* m2_07F80 v1_07F89))) m0_07F7E)] (/ (- (+ (+ (* m0_07F75 b0) (* m3_07F78 b1)) (* m6_07F7B b2)) (+ (* m1_07F7F v2_07F8A) (* m2_07F80 v1_07F89))) m0_07F7E) (/ (- (+ (+ (* m1_07F76 b0) (* m4_07F79 b1)) (* m7_07F7C b2)) (* m5_07F83 v2_07F8A)) m4_07F82) (/ (+ (+ (* m2_07F77 b0) (* m5_07F7A b1)) (* m8_07F7D b2)) m0_07F74)))

(let [s (dmp/s {:op "let"
                :bindings ["asdf" "asdf"]
                :children [(vec (rseq [1 2.0]))]})]
  (dj.math.parser/emit (dmp/s {:op "let"
                               :bindings ["asdf" "asdf"]
                               :children [[ s]]})))