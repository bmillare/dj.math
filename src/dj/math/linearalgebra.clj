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
                   (dm/letm [normu (dmm/vnorm u)]
                            (dm/letm [v (dm/d u normu)]
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
                                                                 R)))))))))))

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
        emitter (dj.math.lispemit/lisp-emitter)]
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
        emitter)))

;; solution [5 3 -2]
#_ (let [[A0 A1 A2 A3 A4 A5 A6 A7 A8 A9] [1.0 1.0 1.0 0.0 2.0 5.0 2.0 5.0 -1.0]
         [b0 b1 b2] [6.0 -4.0 27.0]]
     (let [m0_0F62C (- A0 (Math/copySign (Math/sqrt (+ (+ (clojure.core/* A0 A0) (clojure.core/* A3 A3)) (clojure.core/* A6 A6))) A0)) g_0F62F (Math/sqrt (+ (+ (clojure.core/* m0_0F62C m0_0F62C) (clojure.core/* A3 A3)) (clojure.core/* A6 A6))) m0_0F630 (/ m0_0F62C g_0F62F) m1_0F631 (/ A3 g_0F62F) m2_0F632 (/ A6 g_0F62F) m0_0F633 (+ (+ (* (- (* (* m1_0F631 m0_0F630) 2)) A1) (* (- 1 (* (* m1_0F631 m1_0F631) 2)) A4)) (* (- (* (* m1_0F631 m2_0F632) 2)) A7)) m1_0F634 (+ (+ (* (- (* (* m1_0F631 m0_0F630) 2)) A2) (* (- 1 (* (* m1_0F631 m1_0F631) 2)) A5)) (* (- (* (* m1_0F631 m2_0F632) 2)) A8)) m2_0F635 (+ (+ (* (- (* (* m2_0F632 m0_0F630) 2)) A1) (* (- (* (* m2_0F632 m1_0F631) 2)) A4)) (* (- 1 (* (* m2_0F632 m2_0F632) 2)) A7)) m3_0F636 (+ (+ (* (- (* (* m2_0F632 m0_0F630) 2)) A2) (* (- (* (* m2_0F632 m1_0F631) 2)) A5)) (* (- 1 (* (* m2_0F632 m2_0F632) 2)) A8)) m0_0F637 (- 1 (* (* m0_0F630 m0_0F630) 2)) m1_0F638 (- (* (* m1_0F631 m0_0F630) 2)) m2_0F639 (- (* (* m2_0F632 m0_0F630) 2)) m3_0F63A (- (* (* m0_0F630 m1_0F631) 2)) m4_0F63B (- 1 (* (* m1_0F631 m1_0F631) 2)) m5_0F63C (- (* (* m2_0F632 m1_0F631) 2)) m6_0F63D (- (* (* m0_0F630 m2_0F632) 2)) m7_0F63E (- (* (* m1_0F631 m2_0F632) 2)) m8_0F63F (- 1 (* (* m2_0F632 m2_0F632) 2)) m0_0F640 (Math/copySign (Math/sqrt (+ (+ (clojure.core/* A0 A0) (clojure.core/* A3 A3)) (clojure.core/* A6 A6))) A0) m1_0F641 (+ (+ (* (- 1 (* (* m0_0F630 m0_0F630) 2)) A1) (* (- (* (* m0_0F630 m1_0F631) 2)) A4)) (* (- (* (* m0_0F630 m2_0F632) 2)) A7)) m2_0F642 (+ (+ (* (- 1 (* (* m0_0F630 m0_0F630) 2)) A2) (* (- (* (* m0_0F630 m1_0F631) 2)) A5)) (* (- (* (* m0_0F630 m2_0F632) 2)) A8)) m4_0F644 (+ (+ (* (- (* (* m1_0F631 m0_0F630) 2)) A1) (* (- 1 (* (* m1_0F631 m1_0F631) 2)) A4)) (* (- (* (* m1_0F631 m2_0F632) 2)) A7)) m5_0F645 (+ (+ (* (- (* (* m1_0F631 m0_0F630) 2)) A2) (* (- 1 (* (* m1_0F631 m1_0F631) 2)) A5)) (* (- (* (* m1_0F631 m2_0F632) 2)) A8)) m7_0F647 (+ (+ (* (- (* (* m2_0F632 m0_0F630) 2)) A1) (* (- (* (* m2_0F632 m1_0F631) 2)) A4)) (* (- 1 (* (* m2_0F632 m2_0F632) 2)) A7)) m8_0F648 (+ (+ (* (- (* (* m2_0F632 m0_0F630) 2)) A2) (* (- (* (* m2_0F632 m1_0F631) 2)) A5)) (* (- 1 (* (* m2_0F632 m2_0F632) 2)) A8)) m0_0F649 (- m0_0F633 (Math/copySign (Math/sqrt (+ (clojure.core/* m0_0F633 m0_0F633) (clojure.core/* m2_0F635 m2_0F635))) m0_0F633)) g_0F64B (Math/sqrt (+ (clojure.core/* m0_0F649 m0_0F649) (clojure.core/* m2_0F635 m2_0F635))) m0_0F64C (/ m0_0F649 g_0F64B) m1_0F64D (/ m2_0F635 g_0F64B) m0_0F64E (+ (* (- (* (* m1_0F64D m0_0F64C) 2)) m1_0F634) (* (- 1 (* (* m1_0F64D m1_0F64D) 2)) m3_0F636)) m1_0F650 (+ (* m1_0F638 (- 1 (* (* m0_0F64C m0_0F64C) 2))) (* m2_0F639 (- (* (* m0_0F64C m1_0F64D) 2)))) m2_0F651 (+ (* m1_0F638 (- (* (* m1_0F64D m0_0F64C) 2))) (* m2_0F639 (- 1 (* (* m1_0F64D m1_0F64D) 2)))) m4_0F653 (+ (* m4_0F63B (- 1 (* (* m0_0F64C m0_0F64C) 2))) (* m5_0F63C (- (* (* m0_0F64C m1_0F64D) 2)))) m5_0F654 (+ (* m4_0F63B (- (* (* m1_0F64D m0_0F64C) 2))) (* m5_0F63C (- 1 (* (* m1_0F64D m1_0F64D) 2)))) m7_0F656 (+ (* m7_0F63E (- 1 (* (* m0_0F64C m0_0F64C) 2))) (* m8_0F63F (- (* (* m0_0F64C m1_0F64D) 2)))) m8_0F657 (+ (* m7_0F63E (- (* (* m1_0F64D m0_0F64C) 2))) (* m8_0F63F (- 1 (* (* m1_0F64D m1_0F64D) 2)))) m4_0F65C (Math/copySign (Math/sqrt (+ (clojure.core/* m0_0F633 m0_0F633) (clojure.core/* m2_0F635 m2_0F635))) m0_0F633) m5_0F65D (+ (* (- 1 (* (* m0_0F64C m0_0F64C) 2)) m1_0F634) (* (- (* (* m0_0F64C m1_0F64D) 2)) m3_0F636)) m8_0F660 (+ (* (- (* (* m1_0F64D m0_0F64C) 2)) m1_0F634) (* (- 1 (* (* m1_0F64D m1_0F64D) 2)) m3_0F636)) v2_0F664 (/ (+ (+ (* m2_0F651 b0) (* m5_0F654 b1)) (* m8_0F657 b2)) m0_0F64E) v1_0F663 (/ (- (+ (+ (* m1_0F650 b0) (* m4_0F653 b1)) (* m7_0F656 b2)) (* m5_0F65D v2_0F664)) m4_0F65C) v0_0F662 (/ (- (+ (+ (* m0_0F637 b0) (* m3_0F63A b1)) (* m6_0F63D b2)) (+ (* m2_0F642 v2_0F664) (* m1_0F641 v1_0F663))) m0_0F640)] [v0_0F662 v1_0F663 v2_0F664]))